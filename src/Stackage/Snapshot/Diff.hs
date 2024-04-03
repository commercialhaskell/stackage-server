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
import Data.Aeson.Key
import qualified Data.Text as T (commonPrefixes)
import Data.These
import RIO
import Stackage.Database (GetStackageDatabase, SnapshotId,
                          getPackagesForSnapshotDiff)
import Types
import Web.PathPieces

data WithSnapshotNames a
    = WithSnapshotNames SnapName SnapName a

newtype SnapshotDiff
    = SnapshotDiff { toDiffList :: [(PackageNameP, VersionChange)] }
    deriving (Show, Eq, Generic, Typeable)

instance ToJSON (WithSnapshotNames SnapshotDiff) where
    toJSON (WithSnapshotNames nameA nameB (SnapshotDiff diff)) =
        object [ "comparing" .= [toPathPiece nameA, toPathPiece nameB]
               , "diff"      .= toJSON (map (second (WithSnapshotNames nameA nameB)) diff)
               ]

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
    toJSON (WithSnapshotNames (fromText . toPathPiece -> aKey) (fromText . toPathPiece -> bKey) change) =
        case change of
            VersionChange (This a)    -> object [ aKey .= a ]
            VersionChange (That b)    -> object [ bKey .= b ]
            VersionChange (These a b) -> object [ aKey .= a, bKey .= b ]

getSnapshotDiff :: GetStackageDatabase env m => SnapshotId -> SnapshotId -> m SnapshotDiff
getSnapshotDiff a b = snapshotDiff <$> getPackagesForSnapshotDiff a <*> getPackagesForSnapshotDiff b

snapshotDiff
  :: [(PackageNameP, VersionP)]
  -> [(PackageNameP, VersionP)]
  -> SnapshotDiff
snapshotDiff as0 bs0 =
    SnapshotDiff $ map (second VersionChange) $ go (sortOn cmp as0) (sortOn cmp bs0)
  where
    cmp = toCaseFold . textDisplay . fst

    go as [] = map (second This) as
    go [] bs = map (second That) bs
    go (a:as) (b:bs) =
      case (compare `on` cmp) a b of
        EQ
          | snd a == snd b -> go as bs
          | otherwise -> (fst a, These (snd a) (snd b)) : go as bs
        LT -> (fst a, This $ snd a) : go as (b:bs)
        GT -> (fst b, That $ snd b) : go (a:as) bs
