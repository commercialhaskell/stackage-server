{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
module Stackage.Snapshot.Diff
  ( getSnapshotDiff
  , snapshotDiff
  , SnapshotDiff()
  , toDiffList
  , toVersionedDiffList
  , VersionChange(..)
  , WithSnapshotNames(..)
  ) where

import ClassyPrelude (sortOn, toCaseFold)
import Data.Aeson
import Data.Align
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T (commonPrefixes)
import Data.These
import RIO
import Stackage.Database (GetStackageDatabase, SnapshotId,
                          getPackagesForSnapshot)
import Stackage.Database.Types (PackageListingInfo(..), SnapName)
import Types
import Web.PathPieces

data WithSnapshotNames a
    = WithSnapshotNames SnapName SnapName a

newtype SnapshotDiff
    = SnapshotDiff { unSnapshotDiff :: HashMap PackageNameP VersionChange }
    deriving (Show, Eq, Generic, Typeable)

instance ToJSON (WithSnapshotNames SnapshotDiff) where
    toJSON (WithSnapshotNames nameA nameB (SnapshotDiff diff)) =
        object [ "comparing" .= [toPathPiece nameA, toPathPiece nameB]
               , "diff"      .= toJSON (WithSnapshotNames nameA nameB <$> diff)
               ]

toDiffList :: SnapshotDiff -> [(PackageNameP, VersionChange)]
toDiffList = sortOn (toCaseFold . textDisplay . fst) . HashMap.toList . unSnapshotDiff

versionPrefix :: VersionChange -> Maybe (Text, Text, Text)
versionPrefix vc = case unVersionChange vc of
        These va vb -> T.commonPrefixes (textDisplay va) (textDisplay vb)
        _           -> Nothing

versionedDiffList ::
       [(PackageNameP, VersionChange)] -> [(PackageNameP, VersionChange, Maybe (Text, Text, Text))]
versionedDiffList = map withPrefixedVersion
  where
    withPrefixedVersion (packageName, versionChange) =
        (packageName, versionChange, versionPrefix versionChange)


toVersionedDiffList :: SnapshotDiff -> [(PackageNameP, VersionChange, Maybe (Text, Text, Text))]
toVersionedDiffList = versionedDiffList . toDiffList

-- | Versions of a package as it occurs in the listings provided to `snapshotDiff`.
--
--   Would be represented with `These v1 v2` if the package is present in both listings,
--   otherwise it would be `This v1` if the package is present only in the first listing,
--   or `That v2` if only in the second.
newtype VersionChange = VersionChange { unVersionChange :: These VersionP VersionP }
                      deriving (Show, Eq, Generic, Typeable)

instance ToJSON (WithSnapshotNames VersionChange) where
    toJSON (WithSnapshotNames (toPathPiece -> aKey) (toPathPiece -> bKey) change) =
        case change of
            VersionChange (This a)    -> object [ aKey .= a ]
            VersionChange (That b)    -> object [ bKey .= b ]
            VersionChange (These a b) -> object [ aKey .= a, bKey .= b ]

changed :: VersionChange -> Bool
changed = these (const True) (const True) (/=) . unVersionChange

getSnapshotDiff :: GetStackageDatabase env m => SnapshotId -> SnapshotId -> m SnapshotDiff
getSnapshotDiff a b = snapshotDiff <$> getPackagesForSnapshot a <*> getPackagesForSnapshot b

snapshotDiff :: [PackageListingInfo] -> [PackageListingInfo] -> SnapshotDiff
snapshotDiff as bs =
    SnapshotDiff $ HashMap.filter changed
                 $ alignWith VersionChange (toMap as) (toMap bs)
  where
    toMap = HashMap.fromList . map (pliName &&& pliVersion)
