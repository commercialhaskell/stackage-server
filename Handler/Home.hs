{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Data.Slug
import Database.Esqueleto as E
import Import hiding ((=.),on,(||.),(==.))

-- This is a handler function for the G request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    fpHandle <- mkSlug "fpcomplete"
    stackages <- runDB $ select $ from $ \(stackage `InnerJoin` user) -> do
        on (stackage ^. StackageUser ==. user ^. UserId)
        orderBy [desc $ stackage ^. StackageUploaded]
        where_ (like (user ^. UserDisplay) (val "%@fpcomplete.com") ||.
                  user ^. UserHandle ==. val fpHandle)
        limit 4
        return
            ( stackage ^. StackageIdent
            , stackage ^. StackageTitle
            , stackage ^. StackageUploaded
            , user ^. UserDisplay
            , user ^. UserHandle
            )
    windowsLatest <- linkFor "unstable-ghc78hp-inclusive"
    restLatest    <- linkFor "unstable-ghc78-inclusive"
    defaultLayout $ do
        setTitle "Stackage Server"
        $(widgetFile "homepage")
  where
      linkFor name =
          do slug <- mkSlug name
             selecting (\alias ->
                            do where_ (alias ^. AliasName ==. val slug)
                               return (alias ^. AliasTarget))
        where selecting =
                  fmap (fmap unValue . listToMaybe) .
                  runDB .
                  select .
                  from
                where unValue (Value e) = e
