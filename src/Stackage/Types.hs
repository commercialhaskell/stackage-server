{-# OPTIONS_GHC -fno-warn-orphans #-}
module Stackage.Types
  ( BuildPlan (..)
  , SystemInfo (..)
  , PackagePlan (..)
  , DocMap
  , PackageDocs (..)
  , PackageName
  , Version
  , display
  ) where

import qualified Distribution.Text               as DT
import ClassyPrelude.Conduit
import Data.Aeson
import Distribution.Types.PackageName (PackageName, mkPackageName)
import Distribution.Version (Version)
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Typeable (TypeRep, Typeable, typeOf)

data BuildPlan = BuildPlan
  { bpSystemInfo :: !SystemInfo
  , bpPackages :: !(Map PackageName PackagePlan)
  }
instance FromJSON BuildPlan where
  parseJSON = withObject "BuildPlan" $ \o -> BuildPlan
    <$> o .: "system-info"
    <*> o .: "packages"

data SystemInfo = SystemInfo
  { siGhcVersion :: !Version
  , siCorePackages :: !(Map PackageName Version)
  }
instance FromJSON SystemInfo where
  parseJSON = withObject "SystemInfo" $ \o -> SystemInfo
    <$> o .: "ghc-version"
    <*> o .: "core-packages"

data PackagePlan = PackagePlan
  { ppVersion :: Version
  }
instance FromJSON PackagePlan where
  parseJSON = withObject "PackagePlan" $ \o -> PackagePlan
    <$> o .: "version"

type DocMap = Map Text PackageDocs

data PackageDocs = PackageDocs
    { pdVersion :: !Text
    , pdModules :: !(Map Text [Text])
    }
instance FromJSON PackageDocs where
    parseJSON = withObject "PackageDocs" $ \o -> PackageDocs
        <$> o .: "version"
        <*> o .: "modules"

display :: DT.Text a => a -> Text
display = fromString . DT.display

data ParseFailedException = ParseFailedException TypeRep Text
    deriving (Show, Typeable)
instance Exception ParseFailedException

simpleParse :: (MonadThrow m, DT.Text a, Typeable a) => Text -> m a
simpleParse orig = withTypeRep $ \rep ->
    case DT.simpleParse str of
        Nothing -> throwM (ParseFailedException rep (pack str))
        Just v  -> return v
  where
    str = unpack orig

    withTypeRep :: Typeable a => (TypeRep -> m a) -> m a
    withTypeRep f =
        res
      where
        res = f (typeOf (unwrap res))

        unwrap :: m a -> a
        unwrap _ = error "unwrap"

-- orphans

instance FromJSON Version where
  parseJSON = withText "Version" $ either (fail . show) pure . simpleParse
instance FromJSON PackageName where
  parseJSON = withText "PackageName" $ pure . mkPackageName . unpack
instance FromJSONKey PackageName where
  fromJSONKey = FromJSONKeyText $ mkPackageName . unpack
