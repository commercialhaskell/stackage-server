module Handler.Feed where

import Import
import Stackage.Database

getFeedR :: Handler TypedContent
getFeedR = do
    (_, snaps) <- getSnapshots 20 0
    let entries = flip map snaps $ \snap -> FeedEntry
            { feedEntryLink = SnapshotR (snapshotName snap) StackageHomeR
            , feedEntryUpdated = UTCTime (snapshotCreated snap) 0
            , feedEntryTitle = prettyName (snapshotName snap) (snapshotGhc snap)
            , feedEntryContent = ""
            }
    updated <-
        case entries of
            [] -> liftIO getCurrentTime
            x:_ -> return $ feedEntryUpdated x
    newsFeed Feed
        { feedTitle = "Recent Stackage snapshots"
        , feedLinkSelf = FeedR
        , feedLinkHome = HomeR
        , feedAuthor = "Stackage Project"
        , feedDescription = "Recent Stackage snapshots"
        , feedLanguage = "en"
        , feedUpdated = updated
        , feedEntries = entries
        }
