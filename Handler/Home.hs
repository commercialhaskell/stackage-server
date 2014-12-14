{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Data.Slug
import Database.Esqueleto as E hiding (isNothing)
import Import hiding ((=.),on,(||.),(==.))
import Yesod.GitRepo (grContent)

-- This is a handler function for the G request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = contentHelper "Stackage Server" wcHomepage

getAuthorsR :: Handler Html
getAuthorsR = contentHelper "Library Authors" wcAuthors

getInstallR :: Handler Html
getInstallR = contentHelper "Haskell Installation Instructions" wcInstall

contentHelper :: Html -> (WebsiteContent -> Html) -> Handler Html
contentHelper title accessor = do
    homepage <- getYesod >>= fmap accessor . liftIO . grContent . websiteContent
    defaultLayout $ do
        setTitle title
        toWidget homepage

-- FIXME remove this and switch to above getHomeR' when new homepage is ready
getHomeR' :: Handler Html
getHomeR' = do
    windowsLatest <- linkFor "unstable-ghc78hp-inclusive"
    restLatest    <- linkFor "unstable-ghc78-inclusive"
    defaultLayout $ do
        setTitle "Stackage Server"
        $(combineStylesheets 'StaticR
            [ css_bootstrap_modified_css
            , css_bootstrap_responsive_modified_css
            ])
        $(widgetFile "homepage")
  where
      linkFor name =
          do slug <- mkSlug name
             fpcomplete <- mkSlug "fpcomplete"
             selecting (\(alias, user, stackage) ->
                            do where_ $
                                  alias ^. AliasName ==. val slug &&.
                                  alias ^. AliasUser ==. user ^. UserId &&.
                                  user ^. UserHandle ==. val fpcomplete &&.
                                  alias ^. AliasTarget ==. stackage ^. StackageIdent
                               return (stackage ^. StackageSlug))
        where selecting =
                  fmap (fmap unValue . listToMaybe) .
                  runDB .
                  select .
                  from

      addSnapshot title short = do
          mex <- handlerToWidget $ linkFor $ name "exclusive"
          min' <- handlerToWidget $ linkFor $ name "inclusive"
          when (isJust mex || isJust min')
              [whamlet|
                  <tr>
                     <td>
                         #{asHtml title}
                     <td>
                         $maybe ex <- mex
                           <a href=@{SnapshotR ex StackageHomeR}>exclusive
                         $if isJust mex && isJust min'
                     <td>
                         $maybe in <- min'
                           <a href=@{SnapshotR in StackageHomeR}>inclusive
              |]
        where
          name suffix = concat ["unstable-", short, "-", suffix]
