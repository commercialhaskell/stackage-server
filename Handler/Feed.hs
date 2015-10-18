module Handler.Feed
    ( getFeedR
    , getBranchFeedR
    ) where

import Import
import Stackage.Database
import Data.These
import Stackage.Snapshot.Diff
import Text.Blaze (text)

getFeedR :: Handler TypedContent
getFeedR = getBranchFeed Nothing

getBranchFeedR :: SnapshotBranch -> Handler TypedContent
getBranchFeedR = getBranchFeed . Just

getBranchFeed :: Maybe SnapshotBranch -> Handler TypedContent
getBranchFeed mBranch = mkFeed mBranch =<< getSnapshots mBranch 20 0

mkFeed :: Maybe SnapshotBranch -> [Entity Snapshot] -> Handler TypedContent
mkFeed _ [] = notFound
mkFeed mBranch snaps = do
    entries <- forM snaps $ \(Entity snapid snap) -> do
        content <- getContent snapid snap
        return FeedEntry
            { feedEntryLink = SnapshotR (snapshotName snap) StackageHomeR
            , feedEntryUpdated = UTCTime (snapshotCreated snap) 0
            , feedEntryTitle = prettyName (snapshotName snap) (snapshotGhc snap)
            , feedEntryContent = content
            }
    updated <-
        case entries of
            [] -> liftIO getCurrentTime
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
        }
  where
    branchTitle NightlyBranch = "Nightly"
    branchTitle LtsBranch     = "LTS"
    branchTitle (LtsMajorBranch x) = "LTS-" <> tshow x
    title = "Recent Stackage " <> maybe "" branchTitle mBranch <> " snapshots"

getContent :: SnapshotId -> Snapshot -> Handler Html
getContent sid2 snap = do
    mprev <- snapshotBefore $ snapshotName snap
    case mprev of
        Nothing -> return "No previous snapshot found for comparison"
        Just (sid1, name1) -> do
            snapDiff <- getSnapshotDiff sid1 sid2
            return
                [shamlet|
                  <p>Difference between #{prettyNameShort name1} and #{prettyNameShort $ snapshotName snap}
                  <table border=1 cellpadding=5>
                    <thead>
                      <tr>
                        <th align=right>Package name
                        <th align=right>Old
                        <th align=left>New
                    <tbody>
                      $forall (PackageName name, VersionChange change) <- toDiffList snapDiff
                        <tr>
                          <th align=right>#{name}
                          $case change
                            $of This (Version old)
                              <td align=right>#{old}
                              <td>
                            $of That (Version new)
                              <td align=right>
                              <td>#{new}
                            $of These (Version old) (Version new)
                              <td align=right>#{old}
                              <td>#{new}
                |]
