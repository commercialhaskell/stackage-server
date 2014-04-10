{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import qualified Database.Esqueleto as E

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    stackages <- runDB $ E.select $ E.from $ \(stackage `E.InnerJoin` user) -> do
        E.on (stackage E.^. StackageUser E.==. user E.^. UserId)
        E.orderBy [E.desc $ stackage E.^. StackageUploaded]
        return
            ( stackage E.^. StackageIdent
            , stackage E.^. StackageTitle
            , stackage E.^. StackageUploaded
            , user E.^. UserDisplay
            , user E.^. UserHandle
            )
    defaultLayout $ do
        setTitle "Stackage Server"
        $(widgetFile "homepage")
