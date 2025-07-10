{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Snapshots (getAllSnapshotsR, getApiV1SnapshotsR) where

import RIO.Time
import Import
import Stackage.Database

snapshotsPerPage :: Integral a => a
snapshotsPerPage = 50

-- | Extracted from an earlier implementation that just used a big tuple.
data SnapshotInfo = SnapshotInfo
    { name :: SnapName
    , title :: Text
    , prettyDate :: Text
    } deriving (Eq, Show)

instance ToJSON SnapshotInfo where
    toJSON (SnapshotInfo name title prettyDate) =
        array
            [ toJSON name
            , toJSON title
            , toJSON prettyDate
            ]

-- | Extracted from an earlier implementation that just used a big tuple.
data Paging = Paging
    { totalCount :: Int
    , currentPage :: Int
    , isFirstPage :: Bool
    , isLastPage :: Bool
    } deriving (Eq, Show)

-- | Fetch snapshot data from the DB that is used in these routes.
fetchSnapshots :: Handler ([[SnapshotInfo]], Paging)
fetchSnapshots = do
    cacheSeconds $ 60 * 60 * 6

    currentPageMay <- lookupGetParam "page"
    let currentPage :: Int
        currentPage = fromMaybe 1 (currentPageMay >>= readMay)

    totalCount <- countSnapshots Nothing

    snapshots <- map entityVal <$>
        getSnapshots
            Nothing
            snapshotsPerPage
            ((fromIntegral currentPage - 1) * snapshotsPerPage)

    now' <- getCurrentTime
    let groups = groupUp now' snapshots

    let isFirstPage = currentPage == 1
        isLastPage = currentPage * snapshotsPerPage >= totalCount

    pure (groups, Paging totalCount currentPage isFirstPage isLastPage)

  where uncrapify now' snapshot =
            SnapshotInfo
                (snapshotName snapshot)
                (snapshotTitle snapshot)
                (dateDiff now' (snapshotCreated snapshot))
        groupUp now' = groupBy (on (==) (.prettyDate))
                     . map (uncrapify now')

getAllSnapshotsR :: Handler Html
getAllSnapshotsR = track "Handler.Snapshots.getAllSnapshotsR" $ do
    (groups, Paging _ currentPage isFirstPage isLastPage) <- fetchSnapshots
    defaultLayout $ do
        setTitle "Stackage Server"
        let snapshotsNav = $(widgetFile "snapshots-nav")
        $(widgetFile "all-snapshots")

getApiV1SnapshotsR :: Handler Value
getApiV1SnapshotsR = track "Handler.API.getApiV1SnapshotsR" $ do
    (groups, paging) <- fetchSnapshots
    pure $ object
        [ "snapshots" .= groups
        , "totalCount" .= paging.totalCount
        ]
