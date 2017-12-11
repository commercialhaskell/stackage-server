module Data.WebsiteContent
    ( WebsiteContent (..)
    , StackRelease (..)
    , loadWebsiteContent
    ) where

import ClassyPrelude.Yesod
import Text.Markdown (markdown, msXssProtect, msAddHeadingId)
import Data.GhcLinks
import Data.Aeson (withObject)
import Data.Yaml

data WebsiteContent = WebsiteContent
    { wcHomepage :: !Html
    , wcAuthors  :: !Html
    , wcOlderReleases :: !Html
    , wcGhcLinks :: !GhcLinks
    , wcStackReleases :: ![StackRelease]
    }

loadWebsiteContent :: FilePath -> IO WebsiteContent
loadWebsiteContent dir = do
    wcHomepage <- readHtml "homepage.html"
    wcAuthors <- readHtml "authors.html"
    wcOlderReleases <- readHtml "older-releases.html" `catchIO`
                    \_ -> readMarkdown "older-releases.md"
    wcGhcLinks <- readGhcLinks $ dir </> "stackage-cli"
    wcStackReleases <- decodeFileEither (dir </> "stack" </> "releases.yaml")
        >>= either throwIO return
    return WebsiteContent {..}
  where
    readHtml fp = fmap (preEscapedToMarkup . decodeUtf8 :: ByteString -> Html)
                $ readFile $ dir </> fp
    readMarkdown fp = fmap (markdown def
                        { msXssProtect   = False
                        , msAddHeadingId = True
                        } . fromStrict . decodeUtf8)
               $ readFile $ dir </> fp

data StackRelease = StackRelease
    { srName :: !Text
    , srPattern :: !Text
    }
instance FromJSON StackRelease where
    parseJSON = withObject "StackRelease" $ \o -> StackRelease
        <$> o .: "name"
        <*> o .: "pattern"
