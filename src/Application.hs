{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Application
    ( App
    , withApplicationDev
    , withFoundationDev
    , makeApplication
    , appMain
    , develMain
    , withFoundation
    , makeLogWare
    -- * for DevelMain
    , withApplicationRepl
    -- * for GHCI
    , handler
    ) where

import Control.AutoUpdate
import Control.Concurrent (threadDelay)
import Control.Monad.Logger (liftLoc)
import Data.WebsiteContent
import Database.Persist.Postgresql (PostgresConf(..))
import Import hiding (catch)
import Language.Haskell.TH.Syntax (qLocation)
import Network.Wai (Middleware, rawPathInfo, pathInfo, responseBuilder)
import Network.Wai.Handler.Warp (Settings, defaultSettings,
                                 defaultShouldDisplayException, getPort,
                                 runSettings, setHost, setOnException, setPort)
import Network.Wai.Middleware.ForceSSL (forceSSL)
import Network.Wai.Middleware.RequestLogger (Destination(Logger),
                                             IPAddrSource(..), OutputFormat(..),
                                             destination, mkRequestLogger,
                                             outputFormat)
import RIO (LogFunc, LogOptions, logOptionsHandle, withLogFunc, runRIO, logError, displayShow)
import RIO.Prelude.Simple (runSimpleApp)
import Stackage.Database (withStackageDatabase)
import Stackage.Database.Cron (newHoogleLocker, singleRun)
import Stackage.Database.Github (getStackageContentDir)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)
import Yesod.Core.Types (loggerSet)
import Yesod.Default.Config2
import Yesod.Default.Handlers
import Yesod.GitRepo
import Yesod.GitRev (tGitRev)

-- Import all relevant handler modules here.
import Handler.Blog
import Handler.BuildPlan
import Handler.Download
import Handler.DownloadStack
import Handler.Feed
import Handler.Haddock
import Handler.Home
import Handler.Hoogle
import Handler.MirrorStatus
import Handler.OldLinks
import Handler.Package
import Handler.PackageDeps
import Handler.PackageList
import Handler.Sitemap
import Handler.Snapshots
import Handler.StackageHome
import Handler.StackageIndex
import Handler.StackageSdist
import Handler.System

--import Network.Wai.Middleware.Prometheus (prometheus)
--import Prometheus (register)
--import Prometheus.Metric.GHC (ghcMetrics)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation

    let middleware = id -- prometheus def
                   . healthz
#if !DEVELOPMENT
                   . forceSSL' (appSettings foundation)
#endif
                   . logWare
                   . defaultMiddlewaresNoLogging

    -- FIXME prometheus void (register ghcMetrics)

    return (middleware appPlain)

-- | Bypass any overhead from Yesod
healthz :: Middleware
healthz app req send =
  case pathInfo req of
    ["healthz"] -> send $ responseBuilder status200 [("content-type", "text/plain; charset=utf-8")] "OK"
    _ -> app req send

forceSSL' :: AppSettings -> Middleware
forceSSL' settings app
    | appForceSsl settings = \req send ->
        -- Don't force SSL for tarballs, to provide 00-index.tar.gz and package
        -- tarball access for cabal-install
        if ".tar.gz" `isSuffixOf` rawPathInfo req
            then app req send
            else forceSSL app req send
    | otherwise = app

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
--
-- Some basic initializations: HTTP connection manager, logger, and static
-- subsite.
withFoundation :: LogFunc -> AppSettings -> (App -> IO a) -> IO a
withFoundation appLogFunc appSettings inner = do
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings
             then staticDevel
             else static)
            (appStaticDir appSettings)
    appWebsiteContent <-
        if appDevDownload appSettings
            then do
                fp <- runSimpleApp $ getStackageContentDir "."
                gitRepoDev fp loadWebsiteContent
            else gitRepo "https://github.com/fpco/stackage-content.git" "master" loadWebsiteContent
    let pgConf =
            PostgresConf {pgPoolSize = appPostgresPoolsize appSettings, pgConnStr = encodeUtf8 $ appPostgresString appSettings}
        -- Temporary workaround to force content updates regularly, until
        -- distribution of webhooks is handled via consul
        runContentUpdates =
            Concurrently $
            forever $
            void $ do
                threadDelay $ 1000 * 1000 * 60 * 5
                handleAny (runRIO appLogFunc . RIO.logError . fromString . displayException) $
                    grRefresh appWebsiteContent
    withStackageDatabase (appShouldLogAll appSettings) pgConf $ \appStackageDatabase -> do
        appLatestStackMatcher <-
            mkAutoUpdateWithModify
                defaultUpdateSettings
                    { updateFreq = 1000 * 1000 * 60 * 30 -- update every thirty minutes
                    , updateAction = getLatestMatcher appHttpManager
                    }
                \oldMatcher -> getLatestMatcher appHttpManager `catchAny` \e -> do
                  runRIO appLogFunc $ RIO.logError $ "Couldn't get Stack matcher: " <> displayShow e
                  pure oldMatcher
        appHoogleLock <- newMVar ()
        appMirrorStatus <- mkUpdateMirrorStatus
        hoogleLocker <- newHoogleLocker appLogFunc appHttpManager
        let appGetHoogleDB = singleRun hoogleLocker
        let appGitRev = $$tGitRev
        runConcurrently $ runContentUpdates *> Concurrently (inner App {..})

getLogOpts :: AppSettings -> IO LogOptions
getLogOpts settings = logOptionsHandle stdout (appShouldLogAll settings)


makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, apply an action to Warp settings, RIO's LogFunc and Foundation.
withFoundationDev :: (Settings -> App -> IO a) -> IO a
withFoundationDev inner = do
    appSettings <- getAppSettings
    logOpts <- getLogOpts appSettings
    withLogFunc logOpts $ \logFunc ->
        withFoundation logFunc appSettings $ \foundation -> do
            settings <- getDevSettings $ warpSettings foundation
            inner settings foundation


withApplicationDev :: (Settings -> Application -> IO a) -> IO a
withApplicationDev inner =
    withFoundationDev $ \ settings foundation -> do
        application <- makeApplication foundation
        inner settings application

-- | main function for use by yesod devel
develMain :: IO ()
develMain = withApplicationDev $ \settings app -> develMainHelper (pure (settings, app))

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv
    logOpts <- getLogOpts settings
    withLogFunc logOpts $ \ logFunc -> do
        -- Generate the foundation from the settings
        withFoundation logFunc settings $ \ foundation -> do

            -- Generate a WAI Application from the foundation
            app <- makeApplication foundation

            -- Run the application with Warp
            runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
withApplicationRepl :: (Int -> App -> Application -> IO ()) -> IO ()
withApplicationRepl inner = do
    settings <- getAppSettings
    logOpts <- getLogOpts settings
    withLogFunc logOpts $ \ logFunc ->
        withFoundation logFunc settings $ \foundation -> do
            wsettings <- getDevSettings $ warpSettings foundation
            app1 <- makeApplication foundation
            inner (getPort wsettings) foundation app1


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = do
    logOpts <- logOptionsHandle stdout True
    withLogFunc logOpts $ \ logFunc -> do
        settings <- getAppSettings
        withFoundation logFunc settings (`unsafeHandler` h)
