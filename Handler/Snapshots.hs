{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Snapshots where

import           Data.Time.Clock
import qualified Database.Esqueleto as E
import           Formatting
import           Formatting.Time
import           Import

snapshotsPerPage :: Integral a => a
snapshotsPerPage = 50

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getAllSnapshotsR :: Handler Html
getAllSnapshotsR = do
    error "getAllSnapshotsR"
    {-
    now' <- liftIO getCurrentTime
    currentPageMay <- lookupGetParam "page"
    let currentPage :: Int
        currentPage = fromMaybe 1 (currentPageMay >>= readMay)
    (totalCount, groups) <- fmap (groupUp now') $ runDB $ do
        c <- count ([] :: [Filter Stackage])
        rs <- E.select $ E.from $ \(stackage `E.InnerJoin` user) -> do
            E.on (stackage E.^. StackageUser E.==. user E.^. UserId)
            E.orderBy [E.desc $ stackage E.^. StackageUploaded]
            E.limit snapshotsPerPage
            E.offset ((fromIntegral currentPage - 1) * snapshotsPerPage)
            return
                ( stackage E.^. StackageSlug
                , stackage E.^. StackageTitle
                , stackage E.^. StackageUploaded
                , user E.^. UserDisplay
                , user E.^. UserHandle
                )
        return (c, rs)

    let isFirstPage = currentPage == 1
        isLastPage = currentPage * snapshotsPerPage >= totalCount

    defaultLayout $ do
        setTitle "Stackage Server"
        let snapshotsNav = $(widgetFile "snapshots-nav")
        $(widgetFile "all-snapshots")
  where uncrapify now' c =
            let (E.Value ident, E.Value title, E.Value uploaded, E.Value display, E.Value handle') = c
            in (ident,title,format (diff True) (diffUTCTime uploaded now'),display,handle')
        groupUp now' (c, rs) = (c, (groupBy (on (==) (\(_,_,uploaded,_,_) -> uploaded)) . map (uncrapify now')) rs)
        -}
