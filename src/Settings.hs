{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import Data.Aeson (Result(..), fromJSON, withObject, (.!=), (.:?))
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap (KeyMap)
#endif
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither', Parser)
import Data.Yaml.Config
import Language.Haskell.TH.Syntax (Exp, Name, Q)
import Network.Wai.Handler.Warp (HostPreference)
import Text.Hamlet
import Yesod.Default.Config2 (configSettingsYml)
import Yesod.Default.Util (WidgetFileSettings, wfsHamletSettings,
                           widgetFileNoReload, widgetFileReload)

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.
    , appDatabase               :: !DatabaseSettings

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining
    , appForceSsl               :: Bool
    -- ^ Force redirect to SSL
    , appDevDownload            :: Bool
    -- ^ Controls how Git and database resources are downloaded (True means less downloading)
    , appDownloadBucketUrl      :: Text
    -- ^ Publicly-accessible URL for the bucket holding Haddock contents.
    }

data DatabaseSettings
    = DSPostgres !Text !(Maybe Int)
    | DSSqlite !Text !Int

parseDatabase
  :: Bool -- ^ is this dev? if so, allow default of SQLite
#if MIN_VERSION_aeson(2,0,0)
  -> KeyMap Value
#else
  -> HashMap Text Value
#endif
  -> Parser DatabaseSettings
parseDatabase isDev o =
    if isDev
       then postgres
       else sqlite <|> postgres
  where
      postgres = DSPostgres
        <$> o .: "postgres-string"
        <*> o .: "postgres-poolsize"

      sqlite = do
          True <- o .: "sqlite"
          pure $ DSSqlite "test.sqlite3" 1

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#if DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        appRoot                   <- (\t -> if null t then Nothing else Just t)
                                 <$> o .:? "approot" .!= ""
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        dev                       <- o .:? "development" .!= defaultDev

        appDatabase               <- if dev then pure (DSSqlite "test.sqlite3" 7) else parseDatabase dev o

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
        appShouldLogAll           <- o .:? "should-log-all"   .!= dev
        appReloadTemplates        <- o .:? "reload-templates" .!= dev
        appMutableStatic          <- o .:? "mutable-static"   .!= dev
        appSkipCombining          <- o .:? "skip-combining"   .!= dev
        appForceSsl               <- o .:? "force-ssl"        .!= not dev
        appDevDownload            <- o .:? "dev-download"     .!= dev
        appDownloadBucketUrl      <- o .:? "download-bucket-url" .!= "https://s3.amazonaws.com/haddock.stackage.org"

        return AppSettings {..}

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either impureThrow id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e          -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings


getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv
