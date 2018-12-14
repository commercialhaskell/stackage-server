{-# LANGUAGE NoImplicitPrelude #-}
module Stackage.Types
  ( BuildPlan (..)
  , SystemInfo (..)
  , PackagePlan (..)
  , DocMap
  , PackageDocs (..)
  ) where

import ClassyPrelude.Conduit
import Data.Aeson
import Pantry.Internal.Stackage (PackageNameP(..), VersionP(..))

data BuildPlan = BuildPlan
    { bpSystemInfo :: !SystemInfo
    , bpPackages :: !(Map PackageNameP PackagePlan)
    }
instance FromJSON BuildPlan where
    parseJSON = withObject "BuildPlan" $ \o -> BuildPlan
        <$> o .: "system-info"
        <*> o .: "packages"

data SystemInfo = SystemInfo
    { siGhcVersion :: !VersionP
    , siCorePackages :: !(Map PackageNameP VersionP)
    }
instance FromJSON SystemInfo where
    parseJSON = withObject "SystemInfo" $ \o -> SystemInfo
        <$> o .: "ghc-version"
        <*> o .: "core-packages"

newtype PackagePlan = PackagePlan
    { ppVersion :: VersionP
    }
instance FromJSON PackagePlan where
    parseJSON = withObject "PackagePlan" $ \o -> PackagePlan <$> o .: "version"

type DocMap = Map Text PackageDocs

data PackageDocs = PackageDocs
    { pdVersion :: !Text
    , pdModules :: !(Map Text [Text])
    }
instance FromJSON PackageDocs where
    parseJSON = withObject "PackageDocs" $ \o -> PackageDocs
        <$> o .: "version"
        <*> o .: "modules"

