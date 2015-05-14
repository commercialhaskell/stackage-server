{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home
    ( getHomeR
    , getAuthorsR
    , getInstallR
    , getOlderReleasesR
    ) where

import Import hiding ((=.),on,(||.),(==.))
import Yesod.GitRepo (grContent)

-- This is a handler function for the G request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = contentHelper "Stackage Server" wcHomepage

getAuthorsR :: Handler Html
getAuthorsR = contentHelper "Library Authors" wcAuthors

getInstallR :: Handler Html
getInstallR = contentHelper "Haskell Installation Instructions" wcInstall

getOlderReleasesR :: Handler Html
getOlderReleasesR = contentHelper "Older Releases" wcOlderReleases

contentHelper :: Html -> (WebsiteContent -> Html) -> Handler Html
contentHelper title accessor = do
    homepage <- getYesod >>= fmap accessor . liftIO . grContent . websiteContent
    defaultLayout $ do
        setTitle title
        toWidget homepage
