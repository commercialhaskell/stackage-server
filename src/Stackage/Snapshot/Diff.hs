{-# LANGUAGE PartialTypeSignatures #-}
module Stackage.Snapshot.Diff
  ( getSnapshotDiff
  , snapshotDiff
  , SnapshotDiff()
  , toDiffList
  , toVersionedDiffList
  , VersionChange(..)
  , WithSnapshotNames(..)
  ) where

import qualified Data.Text as T(commonPrefixes)
import           Data.Align
import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import           Control.Arrow
import           ClassyPrelude
import           Data.These
import           Stackage.Database (SnapshotId, PackageListingInfo(..),
                                    GetStackageDatabase, getPackages)
import           Stackage.Database.Types (SnapName)
import           Types
import           Web.PathPieces

data WithSnapshotNames a
    = WithSnapshotNames SnapName SnapName a

newtype SnapshotDiff
    = SnapshotDiff { unSnapshotDiff :: HashMap PackageName VersionChange }
    deriving (Show, Eq, Generic, Typeable)

instance ToJSON (WithSnapshotNames SnapshotDiff) where
    toJSON (WithSnapshotNames nameA nameB (SnapshotDiff diff)) =
        object [ "comparing" .= [toPathPiece nameA, toPathPiece nameB]
               , "diff"      .= toJSON (WithSnapshotNames nameA nameB <$> diff)
               ]

toDiffList :: SnapshotDiff -> [(PackageName, VersionChange)]
toDiffList = sortOn (toCaseFold . unPackageName . fst) . HashMap.toList . unSnapshotDiff

versionPrefix :: VersionChange -> Maybe (Text,Text,Text)
versionPrefix vc = case unVersionChange vc of
        These (Version a) (Version b) -> T.commonPrefixes a b
        _ -> Nothing

versionedDiffList :: [(PackageName, VersionChange)] -> [(PackageName, VersionChange, Maybe (Text,Text,Text))]
versionedDiffList = map withPrefixedVersion
  where
    withPrefixedVersion (packageName, versionChange) = (packageName, versionChange, versionPrefix versionChange)


toVersionedDiffList :: SnapshotDiff -> [(PackageName, VersionChange, Maybe (Text, Text, Text))]
toVersionedDiffList = versionedDiffList . toDiffList

-- | Versions of a package as it occurs in the listings provided to `snapshotDiff`.
--
--   Would be represented with `These v1 v2` if the package is present in both listings,
--   otherwise it would be `This v1` if the package is present only in the first listing,
--   or `That v2` if only in the second.
newtype VersionChange = VersionChange { unVersionChange :: These Version Version }
                      deriving (Show, Eq, Generic, Typeable)

instance ToJSON (WithSnapshotNames VersionChange) where
    toJSON (WithSnapshotNames (toPathPiece -> aKey) (toPathPiece -> bKey) change) =
        case change of
            VersionChange (This a)    -> object [ aKey .= a ]
            VersionChange (That b)    -> object [ bKey .= b ]
            VersionChange (These a b) -> object [ aKey .= a, bKey .= b ]

changed :: VersionChange -> Bool
changed = these (const True) (const True) (/=) . unVersionChange

getSnapshotDiff :: GetStackageDatabase m => SnapshotId -> SnapshotId -> m SnapshotDiff
getSnapshotDiff a b = snapshotDiff <$> getPackages a <*> getPackages b

snapshotDiff :: [PackageListingInfo] -> [PackageListingInfo] -> SnapshotDiff
snapshotDiff as bs =
    SnapshotDiff $ HashMap.filter changed
                 $ alignWith VersionChange (toMap as) (toMap bs)
  where
    toMap = HashMap.fromList . map (PackageName . pliName &&& Version . pliVersion)
