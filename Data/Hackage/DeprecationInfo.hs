-- | Transforms http://hackage.haskell.org/packages/deprecated.json
-- into model data to be stored in the database.
module Data.Hackage.DeprecationInfo
  ( HackageDeprecationInfo(..)
  , loadDeprecationInfo
  ) where

import ClassyPrelude.Yesod
import Data.Aeson as Aeson
import Model
import Types

data HackageDeprecationInfo = HackageDeprecationInfo {
  deprecations :: [Deprecated],
  suggestions :: [Suggested]
}

instance FromJSON HackageDeprecationInfo where
  parseJSON j = do
    deprecationRecords <- parseJSON j
    return $ HackageDeprecationInfo {
      deprecations = map toDeprecated deprecationRecords,
      suggestions = concatMap toSuggestions deprecationRecords
    }

data DeprecationRecord = DeprecationRecord {
  _deprecatedPackage :: PackageName,
  _deprecatedInFavourOf :: [PackageName]
}

instance FromJSON DeprecationRecord where
  parseJSON = withObject "DeprecationRecord" $ \obj -> do
      package <- PackageName <$> (obj .: "deprecated-package")
      inFavourOf <- map PackageName <$> (obj .: "in-favour-of")
      return $ DeprecationRecord package inFavourOf
    where
      parsePackageName = fmap PackageName

toDeprecated :: DeprecationRecord -> Deprecated
toDeprecated (DeprecationRecord deprecated _) = Deprecated deprecated

toSuggestions :: DeprecationRecord -> [Suggested]
toSuggestions (DeprecationRecord deprecated inFavourOf) =
    map toSuggestion inFavourOf
  where
    toSuggestion favoured = Suggested {
      suggestedPackage = favoured,
      suggestedInsteadOf = deprecated
    }

loadDeprecationInfo ::
  ( HasHttpManager env
  , MonadReader env m
  , MonadThrow m
  , MonadIO m)
  => m (Either String HackageDeprecationInfo)
loadDeprecationInfo = do
    req <- parseUrl "http://hackage.haskell.org/packages/deprecated.json"
    res <- httpLbs req
    return $! Aeson.eitherDecode (responseBody res)
