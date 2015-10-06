module Import
    ( module Import
    ) where

import ClassyPrelude.Yesod as Import
import Foundation as Import
import Settings as Import
import Settings.Development as Import
import Settings.StaticFiles as Import
import Types as Import
import Yesod.Auth as Import
import Data.WebsiteContent as Import (WebsiteContent (..))
import Data.Text.Read (decimal)
import Stackage.Database (SnapName)

parseLtsPair :: Text -> Maybe (Int, Int)
parseLtsPair t1 = do
    (x, t2) <- either (const Nothing) Just $ decimal t1
    t3 <- stripPrefix "." t2
    (y, "") <- either (const Nothing) Just $ decimal t3
    Just (x, y)

haddockUrl :: SnapName
           -> Text -- ^ package-version
           -> Text -- ^ module name
           -> Route App
haddockUrl sname pkgver name = HaddockR sname
    [ pkgver
    , omap toDash name ++ ".html"
    ]
  where
    toDash '.' = '-'
    toDash c = c
