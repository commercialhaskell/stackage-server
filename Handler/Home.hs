{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Home
    ( getHomeR
    , getAuthorsR
    , getInstallR
    , getOlderReleasesR
    ) where

import Data.Time.Clock
import Import
import Stackage.Database
import Yesod.GitRepo (grContent)

-- This is a handler function for the G request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = track "Handler.Snapshots.getAllSnapshotsR" $ do
    now' <- liftIO getCurrentTime
    currentPageMay <- lookupGetParam "page"
    let currentPage :: Int
        currentPage = fromMaybe 1 (currentPageMay >>= readMay)
    (map entityVal -> snapshots) <-
        getSnapshots Nothing snapshotsPerPage
                             ((fromIntegral currentPage - 1) * snapshotsPerPage)
    let groups = groupUp now' snapshots
    latestLtsByGhc <- getLatestLtsByGhc
    defaultLayout $ do
        setTitle "Stackage Server"
        $(widgetFile "home")
  where uncrapify now' snapshot =
            ( snapshotName snapshot
            , snapshotTitle snapshot
            , dateDiff now' (snapshotCreated snapshot)
            )
        groupUp now' = groupBy (on (==) (\(_,_,uploaded) -> uploaded))
                     . map (uncrapify now')

snapshotsPerPage :: Int
snapshotsPerPage = 8

getAuthorsR :: Handler Html
getAuthorsR = contentHelper "Library Authors" wcAuthors

getInstallR :: Handler ()
getInstallR = redirect ("https://haskell-lang.org/get-started" :: Text)

getOlderReleasesR :: Handler Html
getOlderReleasesR = contentHelper "Older Releases" wcOlderReleases

contentHelper :: Html -> (WebsiteContent -> Html) -> Handler Html
contentHelper title accessor = do
    homepage <- getYesod >>= fmap accessor . liftIO . grContent . appWebsiteContent
    defaultLayout $ do
        setTitle title
        toWidget homepage
