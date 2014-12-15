module Data.WebsiteContent
    ( WebsiteContent (..)
    , loadWebsiteContent
    ) where

import ClassyPrelude.Yesod
import Text.Markdown (markdown, msXssProtect, msAddHeadingId)

data WebsiteContent = WebsiteContent
    { wcHomepage :: !Html
    , wcAuthors  :: !Html
    , wcInstall  :: !Html
    , wcOlderReleases :: !Html
    }

loadWebsiteContent :: FilePath -> IO WebsiteContent
loadWebsiteContent dir = do
    wcHomepage <- readHtml "homepage.html"
    wcAuthors <- readHtml "authors.html"
    wcInstall <- readMarkdown "install.md"
    wcOlderReleases <- readHtml "older-releases.html" `catchIO`
                    \_ -> readMarkdown "older-releases.md"
    return WebsiteContent {..}
  where
    readHtml fp = fmap (preEscapedToMarkup :: Text -> Html)
                $ readFile $ dir </> fp
    readMarkdown fp = fmap (markdown def
                        { msXssProtect   = False
                        , msAddHeadingId = True
                        })
               $ readFile $ dir </> fp
