module Handler.Alias where

import Import
import Data.Slug (Slug)

handleAliasR :: Slug -> Slug -> [Text] -> Handler ()
handleAliasR user name pieces = do
    $logDebug $ tshow (user, name, pieces)
    Entity _ (Alias _ _ setid) <- runDB $ do
        Entity uid _ <- getBy404 $ UniqueHandle user
        getBy404 $ UniqueAlias uid name
    $logDebug $ "setid: " ++ tshow (setid, pieces)
    case parseRoute ("stackage" : toPathPiece setid : pieces, []) of
        Nothing -> notFound
        Just route -> redirect (route :: Route App)
