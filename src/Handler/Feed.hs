{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Feed
    ( getFeedR
    , getBranchFeedR
    ) where

import Data.These
import Import
import RIO.Time (getCurrentTime)
import Stackage.Database
import Stackage.Snapshot.Diff
import Text.Blaze (text)

getFeedR :: Handler TypedContent
getFeedR = track "Handler.Feed.getBranchFeedR" $ getBranchFeed Nothing

getBranchFeedR :: SnapshotBranch -> Handler TypedContent
getBranchFeedR = track "Handler.Feed.getBranchFeedR" . getBranchFeed . Just

getBranchFeed :: Maybe SnapshotBranch -> Handler TypedContent
getBranchFeed mBranch = do
  cacheSeconds 3600
  mkFeed mBranch =<< getSnapshots mBranch 20 0

mkFeed :: Maybe SnapshotBranch -> [Entity Snapshot] -> Handler TypedContent
mkFeed _ [] = notFound
mkFeed mBranch snaps = do
    entries <- forM snaps $ \(Entity snapid snap) -> do
        showsDiff <- doesShowDiff
        content <-
          if showsDiff
            then getContent snapid snap
            else return mempty
        return FeedEntry
            { feedEntryLink = SnapshotR (snapshotName snap) StackageHomeR
            , feedEntryUpdated = UTCTime (snapshotCreated snap) 0
            , feedEntryTitle = snapshotTitle snap
            , feedEntryContent = content
            , feedEntryEnclosure = Nothing
            , feedEntryCategories = []
            }
    updated <-
        case entries of
            []  -> getCurrentTime
            x:_ -> return $ feedEntryUpdated x
    newsFeed Feed
        { feedTitle = title
        , feedLinkSelf = FeedR
        , feedLinkHome = HomeR
        , feedAuthor = "Stackage Project"
        , feedDescription = text title
        , feedLanguage = "en"
        , feedUpdated = updated
        , feedEntries = entries
        , feedLogo = Nothing
        }
  where
    branchTitle NightlyBranch      = "Nightly"
    branchTitle LtsBranch          = "LTS"
    branchTitle (LtsMajorBranch x) = "LTS-" <> tshow x
    title = "Recent Stackage " <> maybe "" branchTitle mBranch <> " snapshots"

    doesShowDiff =
        (fmap fromPathPiece <$> lookupGetParam "withDiff") >>= \case
            Just (Just False) -> return False
            Just (Just True) -> return True
            Just Nothing -> notFound
            Nothing -> return True


getContent :: SnapshotId -> Snapshot -> Handler Html
getContent sid2 snap = do
    mprev <- snapshotBefore $ snapshotName snap
    case mprev of
        Nothing -> return "No previous snapshot found for comparison"
        Just (sid1, name1) -> do
            snapDiff <- getSnapshotDiff sid1 sid2
            let name2 = snapshotName snap
            withUrlRenderer
                [hamlet|
                  <p>Difference between #{snapshotPrettyNameShort name1} and #{snapshotPrettyNameShort $ snapshotName snap}
                  <table border=1 cellpadding=5>
                    <thead>
                      <tr>
                        <th align=right>Package name
                        <th align=right>Old
                        <th align=left>New
                    <tbody>
                      $forall (pkgname, VersionChange change, versionDiff) <- toVersionedDiffList snapDiff
                        <tr>
                          <th align=right>#{pkgname}
                          $case change
                            $of This old
                              <td align=right>
                                <a href=@{packageUrl name1 pkgname old}#changes>
                                  #{old}
                              <td>
                            $of That new
                              <td align=right>
                              <td>
                                <a href=@{packageUrl name2 pkgname new}#changes>
                                  #{new}
                            $of These old new
                              $maybe (common, left, right) <- versionDiff
                                <td align=right>
                                  <a href=@{packageUrl name1 pkgname old}#changes>
                                    #{common}#
                                    <del style="background-color: #fcc">#{left}
                                <td>
                                  <a href=@{packageUrl name2 pkgname new}#changes>
                                    #{common}#
                                    <ins style="background-color: #cfc">#{right}
                              $nothing
                                <td align=right>
                                  <a href=@{packageUrl name1 pkgname old}#changes>
                                    #{old}
                                <td>
                                  <a href=@{packageUrl name2 pkgname new}#changes>
                                    #{new}
                |]
