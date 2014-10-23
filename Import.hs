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
