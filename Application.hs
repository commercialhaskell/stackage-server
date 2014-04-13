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
import Control.Monad.Logger (runLoggingT)
import Control.Monad.Reader (runReaderT)
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
            }

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    -- Start the cabal file loader
    void $ forkIO $ forever $ flip runLoggingT (messageLoggerSource foundation logger) $ do
        when development $ liftIO $ threadDelay $ 5 * 60 * 1000000
        eres <- tryAny $ flip runReaderT foundation $ do
            loadCabalFiles $ \name version mmtime ->
                runResourceT $ flip (Database.Persist.runPool dbconf) p $ do
                    mx <- getBy $ UniqueUploaded name version
                    case mx of
                        Just {} -> return ()
                        Nothing -> do
                            mtime <- lift $ lift mmtime
                            forM_ mtime $ void . insertBy . Uploaded name version
            let views =
                    [ ("pvp", viewPVP)
                    , ("no-bounds", viewNoBounds)
                    , ("unchanged", viewUnchanged)
                    ]
            forM_ views $ \(name, func) ->
                runResourceT $ flip (Database.Persist.runPool dbconf) p $ createView
                    name
                    func
                    (selectSource [] [])
                    (storeWrite $ HackageViewIndex name)
        case eres of
            Left e -> $logError $ tshow e
            Right () -> return ()
        liftIO $ threadDelay $ 30 * 60 * 1000000

    return foundation

instance MonadActive m => MonadActive (SqlPersistT m) where -- FIXME orphan upstream
    monadActive = lift monadActive
deriving instance MonadCatch m => MonadCatch (SqlPersistT m)
instance MonadCatch m => MonadCatch (ResourceT m) where
  catch (ResourceT m) c = ResourceT $ \r -> m r `catch` \e -> unResourceT (c e) r
  mask a = ResourceT $ \e -> mask $ \u -> unResourceT (a $ q u) e
    where q u (ResourceT b) = ResourceT (u . b)
  uninterruptibleMask a =
    ResourceT $ \e -> uninterruptibleMask $ \u -> unResourceT (a $ q u) e
      where q u (ResourceT b) = ResourceT (u . b)
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
