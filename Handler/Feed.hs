module Handler.Feed
    ( getFeedR
    , getLtsFeedR
    , getLtsMajorFeedR
    , getNightlyFeedR
    ) where

import Import
import Stackage.Database
import           Data.These
import Stackage.Snapshot.Diff
import qualified Data.HashMap.Strict as HashMap
import Text.Blaze (text)

getFeedR :: Handler TypedContent
getFeedR = mkFeed "" . snd =<< getSnapshots 20 0

getLtsFeedR :: Handler TypedContent
getLtsFeedR = mkFeed "LTS" . snd =<< getLtsSnapshots 20 0

getLtsMajorFeedR :: LtsMajor -> Handler TypedContent
getLtsMajorFeedR (LtsMajor v) =
    mkFeed ("LTS-" <> tshow v) . snd =<< getLtsMajorSnapshots v 20 0

getNightlyFeedR :: Handler TypedContent
getNightlyFeedR = mkFeed "Nightly" . snd =<< getNightlySnapshots 20 0

mkFeed :: Text -> [Entity Snapshot] -> Handler TypedContent
mkFeed _ [] = notFound
mkFeed branch snaps = do
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
        { feedTitle = "Recent Stackage " <> branch <> " snapshots"
        , feedLinkSelf = FeedR
        , feedLinkHome = HomeR
        , feedAuthor = "Stackage Project"
        , feedDescription = text ("Recent Stackage " <> branch <> " snapshots")
        , feedLanguage = "en"
        , feedUpdated = updated
        , feedEntries = entries
        }

getContent :: SnapshotId -> Snapshot -> Handler Html
getContent sid2 snap = do
    mprev <-
        case snapshotName snap of
            SNLts x y -> ltsBefore x y
            SNNightly day -> nightlyBefore day
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
                      $forall (name, VersionChange verChange) <- sortOn (toCaseFold . fst) $ HashMap.toList snapDiff
                        <tr>
                          <th align=right>#{name}
                          $case verChange
                            $of This oldVersion
                              <td align=right>#{oldVersion}
                              <td>
                            $of That newVersion
                              <td align=right>
                              <td>#{newVersion}
                            $of These oldVersion newVersion
                              <td align=right>#{oldVersion}
                              <td>#{newVersion}
                |]
