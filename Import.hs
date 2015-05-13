module Import
    ( module Import
    ) where

import ClassyPrelude.Yesod as Import
import Foundation as Import
import Model as Import
import Settings as Import
import Settings.Development as Import
import Settings.StaticFiles as Import
import Types as Import
import Yesod.Auth as Import
import Data.Slug (mkSlug)
import Data.WebsiteContent as Import (WebsiteContent (..))
import Data.Text.Read (decimal)
import Stackage.Database (SnapName)

requireAuthIdOrToken :: Handler UserId
requireAuthIdOrToken = do
    mtoken <- lookupHeader "authorization"
    case decodeUtf8 <$> mtoken of
        Nothing -> requireAuthId
        Just token -> do
            case mkSlug token of
                Nothing -> invalidArgs ["Invalid token: " ++ token]
                Just token' -> do
                    muser <- runDB $ getBy $ UniqueToken token'
                    case muser of
                        Nothing -> invalidArgs ["Unknown token: " ++ token]
                        Just (Entity uid _) -> return uid

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
