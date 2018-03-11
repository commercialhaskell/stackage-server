{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Stackage.Metadata
    ( PackageInfo (..)
    , Deprecation (..)
    ) where

import           Control.Applicative           ((<$>), (<*>))
import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                object, withObject, (.:), (.=))
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import           Data.Typeable                 (Typeable)
import           Distribution.Types.Version    (Version)
import           Distribution.Package          (PackageName)
import           Distribution.Version          (VersionRange)
import           Prelude                       hiding (pi)
import           Stackage.PackageIndex.Conduit (parseDistText, renderDistText)

data PackageInfo = PackageInfo
    { piLatest          :: !Version
    , piHash            :: !Text
    , piAllVersions     :: !(Set Version)
    , piSynopsis        :: !Text
    , piDescription     :: !Text
    , piDescriptionType :: !Text
    , piChangeLog       :: !Text
    , piChangeLogType   :: !Text
    , piBasicDeps       :: !(Map PackageName VersionRange)
    , piTestBenchDeps   :: !(Map PackageName VersionRange)
    , piAuthor          :: !Text
    , piMaintainer      :: !Text
    , piHomepage        :: !Text
    , piLicenseName     :: !Text
    }
    deriving (Show, Eq, Typeable)
instance ToJSON PackageInfo where
    toJSON pi = object
        [ "latest" .= renderDistText (piLatest pi)
        , "hash" .= piHash pi
        , "all-versions" .= map renderDistText (Set.toList $ piAllVersions pi)
        , "synopsis" .= piSynopsis pi
        , "description" .= piDescription pi
        , "description-type" .= piDescriptionType pi
        , "changelog" .= piChangeLog pi
        , "changelog-type" .= piChangeLogType pi
        , "basic-deps" .= showM (piBasicDeps pi)
        , "test-bench-deps" .= showM (piTestBenchDeps pi)
        , "author" .= piAuthor pi
        , "maintainer" .= piMaintainer pi
        , "homepage" .= piHomepage pi
        , "license-name" .= piLicenseName pi
        ]
      where
        showM = Map.mapKeysWith const renderDistText . Map.map renderDistText
instance FromJSON PackageInfo where
    parseJSON = withObject "PackageInfo" $ \o -> PackageInfo
        <$> (o .: "latest" >>= parseDistText)
        <*> o .: "hash"
        <*> (o .: "all-versions" >>= fmap Set.fromList . mapM parseDistText)
        <*> o .: "synopsis"
        <*> o .: "description"
        <*> o .: "description-type"
        <*> o .: "changelog"
        <*> o .: "changelog-type"
        <*> (o .: "basic-deps" >>= parseM)
        <*> (o .: "test-bench-deps" >>= parseM)
        <*> o .: "author"
        <*> o .: "maintainer"
        <*> o .: "homepage"
        <*> o .: "license-name"
      where
        parseM = fmap Map.fromList . mapM go . Map.toList
        go (name, range) = do
            name' <- parseDistText name
            range' <- parseDistText range
            return (name', range')

data Deprecation = Deprecation
    { depPackage    :: !Text
    , depInFavourOf :: !(Set Text)
    }
instance ToJSON Deprecation where
    toJSON d = object
        [ "deprecated-package" .= depPackage d
        , "in-favour-of" .= depInFavourOf d
        ]
instance FromJSON Deprecation where
    parseJSON = withObject "Deprecation" $ \o -> Deprecation
        <$> o .: "deprecated-package"
        <*> o .: "in-favour-of"
