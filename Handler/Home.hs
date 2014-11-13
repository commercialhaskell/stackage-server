{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Data.Slug
import Database.Esqueleto as E hiding (isNothing)
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
             selecting (\(alias, user) ->
                            do where_ $
                                  alias ^. AliasName ==. val slug &&.
                                  alias ^. AliasUser ==. user ^. UserId &&.
                                  user ^. UserHandle ==. val fpcomplete
                               return (alias ^. AliasTarget))
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
                           <a href=@{StackageHomeR ex}>exclusive
                         $if isJust mex && isJust min'
                     <td>
                         $maybe in <- min'
                           <a href=@{StackageHomeR in}>inclusive
              |]
        where
          name suffix = concat ["unstable-", short, "-", suffix]
