-- | Transforms http://hackage.haskell.org/packages/deprecated.json
-- into model data to be stored in the database.
module Data.Hackage.DeprecationInfo
  ( HackageDeprecationInfo(..)
  ) where

import Prelude
import Data.Aeson
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
  parseJSON j = do
      obj <- parseJSON j
      package <- (obj .: "deprecated-package") >>= parsePackageName
      inFavourOf <- (obj .: "in-favour-of") >>= mapM parsePackageName
      return $ DeprecationRecord package inFavourOf
    where
      parsePackageName name = return (PackageName name)

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
