-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql (PostgresConf)
import Yesod.Default.Config
import Yesod.Default.Util
import Data.Yaml
import Settings.Development
import Text.Hamlet
import Data.Aeson (withText, withObject)
import Types

-- | Which Persistent backend this site is using.
type PersistConf = PostgresConf

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: String
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticRoot :: AppConfig DefaultEnv x -> Text
staticRoot conf = [st|#{appRoot conf}/static|]

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

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                             else widgetFileNoReload)
              widgetFileSettings

data Extra = Extra
    { storeConfig :: !BlobStoreConfig
    , hackageRoot :: !HackageRoot
    }
    deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .: "blob-store"
    <*> (HackageRoot <$> o .: "hackage-root")

data BlobStoreConfig = BSCFile !FilePath
                     | BSCAWS !FilePath !Text !Text !Text !Text
    deriving Show

instance FromJSON BlobStoreConfig where
    parseJSON v = file v <|> aws v
      where
        file = withText "BlobStoreConfig" $ \t ->
            case () of
                ()
                    | Just root <- stripPrefix "file:" t -> return $ BSCFile $ fpFromText root
                    | otherwise -> fail $ "Invalid BlobStoreConfig: " ++ show t
        aws = withObject "BlobStoreConfig" $ \o -> BSCAWS
            <$> (fpFromText <$> (o .: "local"))
            <*> o .: "access"
            <*> o .: "secret"
            <*> o .: "bucket"
            <*> o .:? "prefix" .!= ""
