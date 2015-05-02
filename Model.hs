module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Data.Aeson
import Data.Hashable (hashUsing)
import Data.Slug (Slug, SnapSlug)
import qualified Data.Text as Text
import Types

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


ghcMajorVersionToText :: GhcMajorVersion -> Text
ghcMajorVersionToText (GhcMajorVersion major minor)
  = pack (show major) <> "." <> pack (show minor)

ghcMajorVersionFromText :: Text -> Maybe GhcMajorVersion
ghcMajorVersionFromText t = case Text.splitOn "." t of
  [readMay -> Just major, readMay -> Just minor] ->
    Just $ GhcMajorVersion major minor
  _ -> Nothing

instance Hashable GhcMajorVersion where
  hashWithSalt = hashUsing ghcMajorVersionToText

instance Eq GhcMajorVersion where
  (GhcMajorVersion a b) == (GhcMajorVersion a' b') =
    a == a' && b == b'

instance FromJSON GhcMajorVersion where
  parseJSON = withText "GhcMajorVersion" $
    maybe mzero return . ghcMajorVersionFromText

instance ToJSON GhcMajorVersion where
  toJSON = toJSON . ghcMajorVersionToText
