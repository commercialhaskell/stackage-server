{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Handler.Snapshots where

import RIO.Time
import Import
import Stackage.Database

snapshotsPerPage :: Integral a => a
snapshotsPerPage = 50

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getAllSnapshotsR :: Handler TypedContent
getAllSnapshotsR = track "Handler.Snapshots.getAllSnapshotsR" $ do
    cacheSeconds $ 60 * 60 * 6
    now' <- getCurrentTime
    currentPageMay <- lookupGetParam "page"
    let currentPage :: Int
        currentPage = fromMaybe 1 (currentPageMay >>= readMay)
    totalCount <- countSnapshots Nothing
    (map entityVal -> snapshots) <-
        getSnapshots Nothing snapshotsPerPage
                             ((fromIntegral currentPage - 1) * snapshotsPerPage)
    let groups = groupUp now' snapshots

    let isFirstPage = currentPage == 1
        isLastPage = currentPage * snapshotsPerPage >= totalCount

    selectRep $ do
      provideRep $ defaultLayout $ do
        setTitle "Stackage Server"
        let snapshotsNav = $(widgetFile "snapshots-nav")
        $(widgetFile "all-snapshots")

      provideRep $ return $ object ["snapshots" .= groups, "totalCount" .= totalCount]

  where uncrapify now' snapshot =
            ( snapshotName snapshot
            , snapshotTitle snapshot
            , dateDiff now' (snapshotCreated snapshot)
            )
        groupUp now' = groupBy (on (==) (\(_,_,uploaded) -> uploaded))
                     . map (uncrapify now')
