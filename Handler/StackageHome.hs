module Handler.StackageHome where

import Import

getStackageHomeR :: PackageSetIdent -> Handler Html
getStackageHomeR ident = do
    (stackage, user) <- runDB $ do
        Entity _ stackage <- getBy404 $ UniqueStackage ident
        user <- get404 $ stackageUser stackage
        return (stackage, user)
    defaultLayout $ do
        setTitle $ toHtml $ stackageTitle stackage
        $(widgetFile "stackage-home")
