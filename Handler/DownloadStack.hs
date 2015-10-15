module Handler.DownloadStack
    ( getDownloadStackListR
    , getDownloadStackR
    , getLatestMatcher
    ) where

import Import
import Yesod.GitRepo
import Data.WebsiteContent
import Data.Aeson.Parser (json)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Monoid (First (..))

getDownloadStackListR :: Handler Html
getDownloadStackListR = do
    releases <- getYesod >>= fmap wcStackReleases . liftIO . grContent . websiteContent
    defaultLayout $ do
        setTitle "Download Stack"
        $(widgetFile "download-stack-list")

getDownloadStackR :: Text -> Handler ()
getDownloadStackR pattern = do
    matcher <- getYesod >>= liftIO . latestStackMatcher
    maybe notFound redirect $ matcher pattern

-- | Creates a function which will find the latest release for a given pattern.
getLatestMatcher :: Manager -> IO (Text -> Maybe Text)
getLatestMatcher man = do
    let req = "https://api.github.com/repos/commercialhaskell/stack/releases/latest"
            { requestHeaders = [("User-Agent", "Stackage Server")]
            }
    val <- flip runReaderT man $ withResponse req
        $ \res -> responseBody res $$ sinkParser json
    return $ \pattern -> do
        let pattern' = pattern ++ "."
        Object top <- return val
        Array assets <- lookup "assets" top
        getFirst $ fold $ map (First . findMatch pattern') assets
  where
    findMatch pattern' (Object o) = do
        String name <- lookup "name" o
        guard $ not $ ".asc" `isSuffixOf` name
        guard $ pattern' `isInfixOf` name
        String url <- lookup "browser_download_url" o
        Just url
    findMatch _ _ = Nothing
