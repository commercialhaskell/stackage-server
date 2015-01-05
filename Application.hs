{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    , cabalLoaderMain
    ) where

import qualified Aws
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception (catch)
import           Control.Monad.Logger (runLoggingT, LoggingT, defaultLogStr)
import           Data.BlobStore (fileStore, storeWrite, cachedS3Store)
import           Data.Hackage
import           Data.Hackage.Views
import           Data.Unpacking (newDocUnpacker, createHoogleDatabases)
import           Data.WebsiteContent
import           Data.Slug (SnapSlug (..), safeMakeSlug, HasGenIO)
import           Data.Time (diffUTCTime)
import qualified Database.Esqueleto as E
import qualified Database.Persist
import           Filesystem (getModified, removeTree)
import           Import hiding (catch)
import           Language.Haskell.TH.Syntax (Loc(..))
import           Network.Wai (Middleware, responseLBS)
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
import           Yesod.GitRepo
import           System.Environment (getEnvironment)
import           Data.BlobStore (HasBlobStore (..), BlobStore)
import           System.IO (hSetBuffering, BufferMode (LineBuffering))
import qualified Data.ByteString as S
import qualified Data.Text as T
import           System.Process (rawSystem)

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
import           Handler.BannedTags
import           Handler.RefreshDeprecated
import           Handler.Hoogle

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
                else Apache FromFallback
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }
    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
        middleware = nicerExceptions . logWare . defaultMiddlewaresNoLogging
    return (middleware app, logFunc)

nicerExceptions :: Middleware
nicerExceptions app req send = catch (app req send) $ \e -> do
    let text = "Exception thrown to Warp: " ++ tshow (e :: SomeException)
    putStrLn text
    send $ responseLBS status500 [("Content-Type", "text/plain")] $
        fromStrict $ encodeUtf8 text

getDbConf :: AppConfig DefaultEnv Extra -> IO Settings.PersistConf
getDbConf conf =
    withYamlEnvironment "config/postgresql.yml" (appEnv conf)
    Database.Persist.loadConfig >>=
    Database.Persist.applyEnv

loadBlobStore :: Manager -> AppConfig DefaultEnv Extra -> IO (BlobStore StoreKey)
loadBlobStore manager conf =
    case storeConfig $ appExtra conf of
        BSCFile root -> return $ fileStore root
        BSCAWS root access secret bucket prefix -> do
            creds <- Aws.Credentials
                <$> pure (encodeUtf8 access)
                <*> pure (encodeUtf8 secret)
                <*> newIORef []
                <*> pure Nothing
            return $ cachedS3Store root creds bucket prefix manager

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: Bool -> AppConfig DefaultEnv Extra -> IO App
makeFoundation useEcho conf = do
    manager <- newManager
    s <- staticSite
    dbconf <- getDbConf conf
    p <- Database.Persist.createPoolConfig dbconf

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

    blobStore' <- loadBlobStore manager conf

    let haddockRootDir' = "/tmp/stackage-server-haddocks2"
    widgetCache' <- newIORef mempty

#if MIN_VERSION_yesod_gitrepo(0,1,1)
    websiteContent' <- if development
        then do
            void $ rawSystem "git"
                [ "clone"
                , "https://github.com/fpco/stackage-content.git"
                ]
            gitRepoDev "stackage-content" loadWebsiteContent
        else gitRepo
            "https://github.com/fpco/stackage-content.git"
            "master"
            loadWebsiteContent
#else
    websiteContent' <- if development
        then do
            void $ rawSystem "git"
                [ "clone"
                , "https://github.com/fpco/stackage-content.git"
                ]
            tmp <- gitRepo "stackage-content" "master" loadWebsiteContent
            return tmp
                { grRefresh = return ()
                , grContent = loadWebsiteContent "stackage-content"
                }
        else gitRepo
            "https://github.com/fpco/stackage-content.git"
            "master"
            loadWebsiteContent
#endif

    env <- getEnvironment

    let runDB' :: (MonadIO m, MonadBaseControl IO m) => SqlPersistT m a -> m a
        runDB' = flip (Database.Persist.runPool dbconf) p
    docUnpacker <- newDocUnpacker haddockRootDir' blobStore' runDB'

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
            , appDocUnpacker = docUnpacker
            , widgetCache = widgetCache'
            , websiteContent = websiteContent'
            }

    let urlRender' = yesodRender foundation (appRoot conf)

    -- Perform database migration using our application's logging settings.
    when (lookup "STACKAGE_SKIP_MIGRATION" env /= Just "1") $
        runResourceT $
        flip runReaderT gen $
        flip runLoggingT (messageLoggerSource foundation logger) $
        flip (Database.Persist.runPool dbconf) p $ do
            runMigration migrateAll
            checkMigration 1 fixSnapSlugs
            checkMigration 2 setCorePackages


    let updateDB = lookup "STACKAGE_CABAL_LOADER" env /= Just "0"
        forceUpdate = lookup "STACKAGE_FORCE_UPDATE" env == Just "1"
        loadCabalFiles' = appLoadCabalFiles updateDB forceUpdate foundation dbconf p

    -- Start the cabal file loader
    ifRunCabalLoader $ forkIO $ forever $ flip runLoggingT (messageLoggerSource foundation logger) $ do
        $logInfoS "CLEANUP" "Cleaning up /tmp"
        now <- liftIO getCurrentTime
        runResourceT $ sourceDirectory "/tmp" $$ mapM_C (cleanupTemp now)
        $logInfoS "CLEANUP" "Cleaning up complete"

        loadCabalFiles'

        liftIO $ createHoogleDatabases blobStore' runDB' putStrLn urlRender'

        liftIO $ threadDelay $ 30 * 60 * 1000000
    return foundation
  where ifRunCabalLoader m =
            if cabalFileLoader
               then void m
               else return ()

data CabalLoaderEnv = CabalLoaderEnv
    { cleSettings :: !(AppConfig DefaultEnv Extra)
    , cleBlobStore :: !(BlobStore StoreKey)
    , cleManager :: !Manager
    }

instance HasHackageRoot CabalLoaderEnv where
    getHackageRoot = hackageRoot . appExtra . cleSettings
instance HasBlobStore CabalLoaderEnv StoreKey where
    getBlobStore = cleBlobStore
instance HasHttpManager CabalLoaderEnv where
    getHttpManager = cleManager

cabalLoaderMain :: IO ()
cabalLoaderMain = do
    conf <- fromArgs parseExtra
    dbconf <- getDbConf conf
    pool <- Database.Persist.createPoolConfig dbconf
    manager <- newManager
    bs <- loadBlobStore manager conf
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let forceUpdate = lookup "STACKAGE_FORCE_UPDATE" env == Just "1"
    flip runLoggingT logFunc $ appLoadCabalFiles
        True -- update database?
        forceUpdate
        CabalLoaderEnv
            { cleSettings = conf
            , cleBlobStore = bs
            , cleManager = manager
            }
        dbconf
        pool

    let foundation = App
            { settings = conf
            , getStatic = error "getStatic"
            , connPool = pool
            , httpManager = manager
            , persistConfig = dbconf
            , appLogger = error "appLogger"
            , genIO = error "genIO"
            , blobStore = bs
            , progressMap = error "progressMap"
            , nextProgressKey = error "nextProgressKey"
            , haddockRootDir = error "haddockRootDir"
            , appDocUnpacker = error "appDocUnpacker"
            , widgetCache = error "widgetCache"
            , websiteContent = error "websiteContent"
            }
    createHoogleDatabases
        bs
        (flip (Database.Persist.runPool dbconf) pool)
        putStrLn
        (yesodRender foundation (appRoot conf))
  where
    logFunc loc src level str
        | level > LevelDebug = S.hPutStr stdout $ fromLogStr $ defaultLogStr loc src level str
        | otherwise = return ()

appLoadCabalFiles :: ( PersistConfig c
                     , PersistConfigBackend c ~ SqlPersistT
                     , HasHackageRoot env
                     , HasBlobStore env StoreKey
                     , HasHttpManager env
                     )
                  => Bool -- ^ update database?
                  -> Bool -- ^ force update?
                  -> env
                  -> c
                  -> PersistConfigPool c
                  -> LoggingT IO ()
appLoadCabalFiles updateDB forceUpdate env dbconf p = do
    eres <- tryAny $ flip runReaderT env $ do
        let runDB' :: SqlPersistT (ResourceT (ReaderT env (LoggingT IO))) a
                   -> ReaderT env (LoggingT IO) a
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
        UploadState uploadHistory newUploads _ newMD <- loadCabalFiles updateDB forceUpdate uploadHistory0 metadata0
        $logInfo "Inserting to new uploads"
        runDB' $ insertMany_ newUploads
        $logInfo $ "Updating metadatas: " ++ tshow (length newMD)
        runDB' $ do
            let newMD' = toList newMD
            deleteWhere [MetadataName <-. map metadataName newMD']
            insertMany_ newMD'
            forM_ newMD' $ \md -> do
                deleteWhere [DependencyUser ==. metadataName md]
                insertMany_ $ flip map (metadataDeps md) $ \dep ->
                    Dependency (PackageName dep) (metadataName md)
        let views =
                [ ("pvp", viewPVP uploadHistory)
                , ("no-bounds", viewNoBounds)
                , ("unchanged", viewUnchanged)
                ]
        forM_ views $ \(name, func) -> do
            $logInfo $ "Generating view: " ++ toPathPiece name
            runResourceT $ createView
                name
                func
                (sourceHistory uploadHistory)
                (storeWrite $ HackageViewIndex name)
    case eres of
        Left e -> $logError $ tshow e
        Right () -> return ()

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

-- for yesod devel
getApplicationDev :: Bool -> IO (Int, Application)
getApplicationDev useEcho =
    defaultDevelApp loader (fmap fst . makeApplication useEcho)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

checkMigration :: MonadIO m
               => Int
               -> ReaderT SqlBackend m ()
               -> ReaderT SqlBackend m ()
checkMigration num f = do
    eres <- insertBy $ Migration num
    case eres of
        Left _ -> return ()
        Right _ -> f

fixSnapSlugs :: (MonadResource m, HasGenIO env, MonadReader env m)
             => ReaderT SqlBackend m ()
fixSnapSlugs =
    selectSource [] [Asc StackageUploaded] $$ mapM_C go
  where
    go (Entity sid Stackage {..}) =
        loop (1 :: Int)
      where
        base = T.replace "haskell platform" "hp"
             $ T.replace "stackage build for " ""
             $ toLower stackageTitle
        loop 50 = error "fixSnapSlugs can't find a good slug"
        loop i = do
            slug' <- lift $ safeMakeSlug base $ if i == 1 then False else True
            let slug = SnapSlug slug'
            ms <- getBy $ UniqueSnapshot slug
            case ms of
                Nothing -> update sid [StackageSlug =. slug]
                Just _ -> loop (i + 1)

setCorePackages :: MonadIO m => ReaderT SqlBackend m ()
setCorePackages =
    updateWhere
        [ PackageName' <-. defaultCorePackages
        , PackageCore ==. Nothing
        ]
        [PackageCore =. Just True]
  where
    defaultCorePackages = map PackageName $ words =<<
        [ "ghc hoopl bytestring unix haskeline Cabal base time xhtml"
        , "haskell98 hpc filepath process array integer-gmp bin-package-db"
        , "containers haskell2010 binary ghc-prim old-time old-locale rts"
        , "terminfo transformers deepseq pretty template-haskell directory"
        ]
