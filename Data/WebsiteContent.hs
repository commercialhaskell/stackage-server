module Data.WebsiteContent
    ( WebsiteContent (..)
    , loadWebsiteContent
    ) where

import ClassyPrelude.Yesod
import Text.Blaze.Html (preEscapedToMarkup)

data WebsiteContent = WebsiteContent
    { wcHomepage :: !Html
    }

loadWebsiteContent :: FilePath -> IO WebsiteContent
loadWebsiteContent dir = do
    wcHomepage <- fmap (preEscapedToMarkup :: Text -> Html)
                $ readFile $ dir </> "homepage.html"
    return WebsiteContent {..}
