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
    }

loadWebsiteContent :: FilePath -> IO WebsiteContent
loadWebsiteContent dir = do
    wcHomepage <- fmap (preEscapedToMarkup :: Text -> Html)
                $ readFile $ dir </> "homepage.html"
    wcAuthors <- fmap (preEscapedToMarkup :: Text -> Html)
               $ readFile $ dir </> "authors.html"
    wcInstall <- fmap (markdown def
                        { msXssProtect   = False
                        , msAddHeadingId = True
                        })
               $ readFile $ dir </> "install.md"
    return WebsiteContent {..}
