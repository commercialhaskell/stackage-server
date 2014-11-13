{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Handler.Snapshots where

import           Data.Time.Clock
import qualified Database.Esqueleto as E
import           Formatting
import           Formatting.Time
import           Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getAllSnapshotsR :: Handler Html
getAllSnapshotsR = do
    now <- liftIO getCurrentTime
    groups <- fmap (groupBy (on (==) (\(_,_,uploaded,_,_) -> uploaded)) . map (uncrapify now)) $
        runDB $ E.select $ E.from $ \(stackage `E.InnerJoin` user) -> do
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
        $(combineStylesheets 'StaticR
            [ css_bootstrap_css
            , css_bootstrap_responsive_css
            ])
        $(widgetFile "all-snapshots")
  where uncrapify now c =
            let (E.Value ident, E.Value title, E.Value uploaded, E.Value display, E.Value handle) = c
            in (ident,title,format (diff True) (diffUTCTime uploaded now),display,handle)
