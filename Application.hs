{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import qualified Aws
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad.Logger (runLoggingT, LoggingT)
import           Control.Monad.Reader (MonadReader (..))
import           Control.Monad.Reader (runReaderT, ReaderT)
import           Control.Monad.Trans.Control
import           Data.BlobStore (fileStore, storeWrite, cachedS3Store)
import           Data.Conduit.Lazy (MonadActive, monadActive)
import           Data.Hackage
import           Data.Hackage.Views
import           Data.Time (diffUTCTime)
import qualified Database.Esqueleto as E
import qualified Database.Persist
import           Filesystem (getModified, removeTree)
import           Import hiding (catch)
import           Language.Haskell.TH.Syntax (Loc(..))
import           Network.Wai.Logger (clockDateCacher)
import           Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import           Settings
import           System.Log.FastLogger (newStdoutLoggerSet, newFileLoggerSet, defaultBufSize, flushLogStr, fromLogStr)
import qualified System.Random.MWC as MWC
import           Yesod.Core.Types (loggerSet, Logger (Logger))
import           Yesod.Default.Config
import           Yesod.Default.Handlers
import           Yesod.Default.Main

import qualified Echo

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Handler.Home
import           Handler.Snapshots
import           Handler.Profile
import           Handler.Email
import           Handler.ResetToken
import           Handler.UploadStackage
import           Handler.StackageHome
import           Handler.StackageIndex
import           Handler.StackageSdist
import           Handler.HackageViewIndex
import           Handler.HackageViewSdist
import           Handler.Aliases
import           Handler.Alias
import           Handler.Progress
import           Handler.System
import           Handler.Haddock
import           Handler.Package
import           Handler.PackageList
import           Handler.CompressorStatus
import           Handler.Tag

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: Bool -- ^ Use Echo.
                -> AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication echo@True conf = do
   foundation <- makeFoundation echo conf
   app <- toWaiAppPlain foundation
   logWare <- mkRequestLogger def
       { destination = RequestLogger.Callback (const (return ()))
       }
   Echo.clear
   return (logWare (defaultMiddlewaresNoLogging app),logFunc)
 where logFunc (Loc filename' _pkg _mod (line,_) _) source level str =
           Echo.write (filename',line) (show source ++ ": " ++ show level ++ ": " ++ toStr str)
       toStr = unpack . decodeUtf8 . fromLogStr
makeApplication echo@False conf = do
    foundation <- makeFoundation echo conf
    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }
    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
        middleware = logWare . defaultMiddlewaresNoLogging
    return (middleware app, logFunc)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: Bool -> AppConfig DefaultEnv Extra -> IO App
makeFoundation useEcho conf = do
    manager <- newManager
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    loggerSet' <- if useEcho
                     then newFileLoggerSet defaultBufSize "/dev/null"
                     else newStdoutLoggerSet defaultBufSize
    (getter, updater) <- clockDateCacher

    -- If the Yesod logger (as opposed to the request logger middleware) is
    -- used less than once a second on average, you may prefer to omit this
    -- thread and use "(updater >> getter)" in place of "getter" below.  That
    -- would update the cache every time it is used, instead of every second.
    let updateLoop = do
            threadDelay 1000000
            updater
            flushLogStr loggerSet'
            updateLoop
    _ <- forkIO updateLoop

    gen <- MWC.createSystemRandom
    progressMap' <- newIORef mempty
    nextProgressKey' <- newIORef 0

    blobStore' <-
        case storeConfig $ appExtra conf of
            BSCFile root -> return $ fileStore root
            BSCAWS root access secret bucket prefix -> do
                creds <- Aws.Credentials
                    <$> pure (encodeUtf8 access)
                    <*> pure (encodeUtf8 secret)
                    <*> newIORef []
                    <*> pure Nothing
                return $ cachedS3Store root creds bucket prefix manager

    let haddockRootDir' = "/tmp/stackage-server-haddocks2"
    (statusRef, unpacker) <- createHaddockUnpacker haddockRootDir' blobStore'
    widgetCache' <- newIORef mempty

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App
            { settings = conf
            , getStatic = s
            , connPool = p
            , httpManager = manager
            , persistConfig = dbconf
            , appLogger = logger
            , genIO = gen
            , blobStore = blobStore'
            , progressMap = progressMap'
            , nextProgressKey = nextProgressKey'
            , haddockRootDir = haddockRootDir'
            , haddockUnpacker = unpacker
            , widgetCache = widgetCache'
            , compressorStatus = statusRef
            }

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    -- Start the cabal file loader
    ifRunCabalLoader $ forkIO $ forever $ flip runLoggingT (messageLoggerSource foundation logger) $ do
        $logInfoS "CLEANUP" "Cleaning up /tmp"
        now <- liftIO getCurrentTime
        runResourceT $ sourceDirectory "/tmp" $$ mapM_C (cleanupTemp now)
        $logInfoS "CLEANUP" "Cleaning up complete"

        --when development $ liftIO $ threadDelay $ 5 * 60 * 1000000
        eres <- tryAny $ flip runReaderT foundation $ do
            let runDB' :: SqlPersistT (ResourceT (ReaderT App (LoggingT IO))) a
                       -> ReaderT App (LoggingT IO) a
                runDB' = runResourceT . flip (Database.Persist.runPool dbconf) p
            uploadHistory0 <- runDB' $ selectSource [] [] $$ sinkUploadHistory
            let toMDPair (E.Value name, E.Value version, E.Value hash') =
                    (name, (version, hash'))
            metadata0 <- fmap (mapFromList . map toMDPair)
                       $ runDB' $ E.select $ E.from $ \m -> return
                ( m E.^. MetadataName
                , m E.^. MetadataVersion
                , m E.^. MetadataHash
                )
            UploadState uploadHistory newUploads _ newMD <- loadCabalFiles uploadHistory0 metadata0
            runDB' $ mapM_ insert_ newUploads
            runDB' $ forM_ newMD $ \x -> do
                deleteBy $ UniqueMetadata $ metadataName x
                insert_ x
            let views =
                    [ ("pvp", viewPVP uploadHistory)
                    , ("no-bounds", viewNoBounds)
                    , ("unchanged", viewUnchanged)
                    ]
            forM_ views $ \(name, func) -> runResourceT $ createView
                    name
                    func
                    (sourceHistory uploadHistory)
                    (storeWrite $ HackageViewIndex name)
        case eres of
            Left e -> $logError $ tshow e
            Right () -> return ()
        liftIO $ threadDelay $ 30 * 60 * 1000000
    return foundation
  where ifRunCabalLoader m =
            if cabalFileLoader
               then void m
               else return ()

cleanupTemp :: UTCTime -> FilePath -> ResourceT (LoggingT IO) ()
cleanupTemp now fp
    | any (`isPrefixOf` name) prefixes = handleAny ($logError . tshow) $ do
        modified <- liftIO $ getModified fp
        if (diffUTCTime now modified > 60 * 60)
          then do
            $logInfoS "CLEANUP" $ "Removing temp directory: " ++ fpToText fp
            liftIO $ removeTree fp
            $logInfoS "CLEANUP" $ "Temp directory deleted: " ++ fpToText fp
          else $logInfoS "CLEANUP" $ "Ignoring recent entry: " ++ fpToText fp
    | otherwise = $logInfoS "CLEANUP" $ "Ignoring unmatched path: " ++ fpToText fp
  where
    name = fpToText $ filename fp
    prefixes = asVector $ pack
        [ "hackage-index"
        , "createview"
        , "build00index."
        , "newindex"
        ]

instance MonadActive m => MonadActive (SqlPersistT m) where -- FIXME orphan upstream
    monadActive = lift monadActive
instance MonadReader env m => MonadReader env (SqlPersistT m) where
    ask = lift ask
    local f m =
        do stT <- liftWith (\run -> local f (run m))
           restoreT (return stT)

-- for yesod devel
getApplicationDev :: Bool -> IO (Int, Application)
getApplicationDev useEcho =
    defaultDevelApp loader (fmap fst . makeApplication useEcho)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
