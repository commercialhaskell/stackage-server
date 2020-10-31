{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Handler.Home
    ( getHomeR
    , getHealthzR
    , getAuthorsR
    , getInstallR
    , getOlderReleasesR
    ) where

import RIO.Time
import Import
import Stackage.Database
import Yesod.GitRepo (grContent)

getHealthzR :: Handler String
getHealthzR = return "This should never be used, we should use the middleware instead"

-- This is a handler function for the G request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = track "Handler.Snapshots.getAllSnapshotsR" $ do
    cacheSeconds $ 60 * 60
    now' <- getCurrentTime
    (map entityVal -> nightly) <-
        getSnapshots (Just  NightlyBranch) 1 0
    let latestNightly = groupUp now' nightly
    (map entityVal -> lts) <-
        getSnapshots (Just  LtsBranch) 1 0
    let latestLts = groupUp now' lts
    latestLtsNameWithHoogle <- getLatestLtsNameWithHoogle
    latestLtsByGhc <- getLatestLtsByGhc

    mrecentBlog <- headMay <$> getPosts

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
