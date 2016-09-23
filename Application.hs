{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    ) where

import Control.Monad.Logger                 (liftLoc)
import Language.Haskell.TH.Syntax           (qLocation)
import           Control.Concurrent (forkIO)
import           Data.WebsiteContent
import           Import hiding (catch)
import           Network.Wai (Middleware, rawPathInfo)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import           Network.Wai.Middleware.ForceSSL (forceSSL)
import           Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    , Destination (Logger)
    )
import           System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize, toLogStr)
import           Yesod.Core.Types (loggerSet)
import           Yesod.Default.Config2
import           Yesod.Default.Handlers
import           Yesod.GitRepo
import           System.Process (rawSystem)
import           Stackage.Database.Cron (loadFromS3)
import           Control.AutoUpdate

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Handler.Home
import           Handler.Snapshots
import           Handler.StackageHome
import           Handler.StackageIndex
import           Handler.StackageSdist
import           Handler.System
import           Handler.Haddock
import           Handler.Package
import           Handler.PackageList
import           Handler.Hoogle
import           Handler.BuildVersion
import           Handler.Sitemap
import           Handler.BuildPlan
import           Handler.Download
import           Handler.OldLinks
import           Handler.Feed
import           Handler.DownloadStack
import           Handler.MirrorStatus

import           Network.Wai.Middleware.Prometheus (prometheus)
import           Prometheus (register)
import           Prometheus.Metric.GHC (ghcMetrics)

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

    let middleware = prometheus def
                   . forceSSL' (appSettings foundation)
                   . logWare
                   . defaultMiddlewaresNoLogging

    void (register ghcMetrics)

    return (middleware appPlain)

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
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    appWebsiteContent <- if appDevDownload appSettings
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

    (appStackageDatabase, refreshDB) <- loadFromS3 (appDevDownload appSettings) appHttpManager

    -- Temporary workaround to force content updates regularly, until
    -- distribution of webhooks is handled via consul
    void $ forkIO $ forever $ void $ do
        threadDelay $ 1000 * 1000 * 60 * 5
        handleAny print refreshDB
        handleAny print $ grRefresh appWebsiteContent

    appLatestStackMatcher <- mkAutoUpdate defaultUpdateSettings
        { updateFreq = 1000 * 1000 * 60 * 30 -- update every thirty minutes
        , updateAction = getLatestMatcher appHttpManager
        }

    appHoogleLock <- newMVar ()

    appMirrorStatus <- mkUpdateMirrorStatus

    return App {..}

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

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h
