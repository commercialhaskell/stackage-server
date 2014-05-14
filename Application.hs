{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import hiding (catch)
import Settings
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Database.Persist
import Control.Monad.Logger (runLoggingT, LoggingT)
import Control.Monad.Reader (runReaderT, ReaderT)
import Control.Concurrent (forkIO, threadDelay)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize, flushLogStr)
import Network.Wai.Logger (clockDateCacher)
import Yesod.Core.Types (loggerSet, Logger (Logger))
import qualified System.Random.MWC as MWC
import Data.BlobStore (fileStore, storeWrite)
import Data.Hackage
import Data.Hackage.Views
import Data.Conduit.Lazy (MonadActive, monadActive)
import Control.Monad.Catch (MonadCatch (..))
import Database.Persist.Sql (SqlPersistT (..))
import Control.Monad.Trans.Resource.Internal (ResourceT (..))
import Control.Monad.Reader (MonadReader (..))
import Filesystem (getModified, removeTree)
import Data.Time (diffUTCTime)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Profile
import Handler.Email
import Handler.ResetToken
import Handler.UploadStackage
import Handler.StackageHome
import Handler.StackageIndex
import Handler.StackageSdist
import Handler.HackageViewIndex
import Handler.HackageViewSdist
import Handler.Aliases
import Handler.Alias
import Handler.Progress
import Handler.System

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

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
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    loggerSet' <- newStdoutLoggerSet defaultBufSize
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

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App
            { settings = conf
            , getStatic = s
            , connPool = p
            , httpManager = manager
            , persistConfig = dbconf
            , appLogger = logger
            , genIO = gen
            , blobStore =
                case storeConfig $ appExtra conf of
                    BSCFile root -> fileStore root
            , progressMap = progressMap'
            , nextProgressKey = nextProgressKey'
            }

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    -- Start the cabal file loader
    void $ forkIO $ forever $ flip runLoggingT (messageLoggerSource foundation logger) $ do
        $logInfo "Cleaning up /tmp"
        now <- liftIO getCurrentTime
        runResourceT $ sourceDirectory "/tmp" $$ mapM_C (cleanupTemp now)

        --when development $ liftIO $ threadDelay $ 5 * 60 * 1000000
        eres <- tryAny $ flip runReaderT foundation $ do
            let runDB' :: SqlPersistT (ResourceT (ReaderT App (LoggingT IO))) a
                       -> ReaderT App (LoggingT IO) a
                runDB' = runResourceT . flip (Database.Persist.runPool dbconf) p
            uploadHistory0 <- runDB' $ selectSource [] [] $$ sinkUploadHistory
            UploadState uploadHistory newUploads <- loadCabalFiles uploadHistory0
            runDB' $ mapM_ insert_ newUploads
            let views =
                    [ ("pvp", viewPVP uploadHistory)
                    , ("no-bounds", viewNoBounds)
                    , ("unchanged", viewUnchanged)
                    ]
            forM_ views $ \(name, func) ->
                runResourceT $ flip (Database.Persist.runPool dbconf) p $ createView
                    name
                    func
                    (sourceHistory uploadHistory)
                    (storeWrite $ HackageViewIndex name)
        case eres of
            Left e -> $logError $ tshow e
            Right () -> return ()
        liftIO $ threadDelay $ 30 * 60 * 1000000

    return foundation

cleanupTemp :: UTCTime -> FilePath -> ResourceT (LoggingT IO) ()
cleanupTemp now fp
    | any (`isPrefixOf` name) prefixes = handleAny ($logError . tshow) $ do
        modified <- liftIO $ getModified fp
        when (diffUTCTime now modified > 60 * 60) $ do
            $logInfo $ "Removing temp directory: " ++ fpToText fp
            liftIO $ removeTree fp
            $logInfo $ "Temp directory deleted: " ++ fpToText fp
    | otherwise = return ()
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

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
