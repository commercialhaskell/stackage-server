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

requireDocs :: Entity Stackage -> Handler ()
requireDocs stackageEnt = do
    master <- getYesod
    status <- liftIO $ duRequestDocs (appDocUnpacker master) stackageEnt
    case status of
        USReady -> return ()
        USBusy -> (>>= sendResponse) $ defaultLayout $ do
            setTitle "Docs unpacking, please wait"
            addHeader "Refresh" "1"
            msg <- liftIO $ duGetStatus $ appDocUnpacker master
            [whamlet|
                <div .container>
                    <p>Docs are currently being unpacked, please wait.
                    <p>This page will automatically reload every second.
                    <p>Current status: #{msg}
            |]
        USFailed e -> do
            $logWarn $ "Docs not available: " ++ tshow
                ( stackageSlug $ entityVal stackageEnt
                , e
                )
            invalidArgs
                [ "Docs not available: " ++ e
                ]
