module Handler.Alias
    ( handleAliasR
    , getLtsR
    , getNightlyR
    ) where

import Import
import Data.Slug (Slug)
import Handler.StackageHome (getStackageHomeR, getStackageMetadataR, getStackageCabalConfigR, getSnapshotPackagesR, getDocsR)
import Handler.StackageIndex (getStackageIndexR, getStackageBundleR)
import Handler.StackageSdist (getStackageSdistR)

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

getLtsR :: [Text] -> Handler ()
getLtsR pieces0 =
    case pieces0 of
        [] -> go []
        piece:pieces'
            | Just (x, y) <- parseLtsPair piece -> goXY x y pieces'
            | Just x <- fromPathPiece piece -> goX x pieces'
            | otherwise -> go pieces0
  where
    go pieces = do
        mlts <- runDB $ selectFirst [] [Desc LtsMajor, Desc LtsMinor]
        case mlts of
            Nothing -> notFound
            Just (Entity _ (Lts _ _ sid)) -> goSid sid pieces

    goX x pieces = do
        mlts <- runDB $ selectFirst [LtsMajor ==. x] [Desc LtsMinor]
        case mlts of
            Nothing -> notFound
            Just (Entity _ (Lts _ _ sid)) -> goSid sid pieces

    goXY x y pieces = do
        Entity _ (Lts _ _ sid) <- runDB $ getBy404 $ UniqueLts x y
        goSid sid pieces

getNightlyR :: [Text] -> Handler ()
getNightlyR pieces0 =
    case pieces0 of
        [] -> go []
        piece:pieces'
            | Just day <- fromPathPiece piece -> goDay day pieces'
            | otherwise -> go pieces0
  where
    go pieces = do
        mn <- runDB $ selectFirst [] [Desc NightlyDay]
        case mn of
            Nothing -> notFound
            Just (Entity _ (Nightly _ _ sid)) -> goSid sid pieces
    goDay day pieces = do
        Entity _ (Nightly _ _ sid) <- runDB $ getBy404 $ UniqueNightly day
        goSid sid pieces

goSid :: StackageId -> [Text] -> Handler ()
goSid sid pieces = do
    s <- runDB $ get404 sid
    case parseRoute ("snapshot" : toPathPiece (stackageSlug s) : pieces, []) of
        Just (SnapshotR slug sr) ->
            case sr of
                StackageHomeR -> getStackageHomeR slug >>= sendResponse
                StackageMetadataR -> getStackageMetadataR slug >>= sendResponse
                StackageCabalConfigR -> getStackageCabalConfigR slug >>= sendResponse
                StackageIndexR -> getStackageIndexR slug >>= sendResponse
                StackageBundleR -> getStackageBundleR slug >>= sendResponse
                StackageSdistR pnv -> getStackageSdistR slug pnv >>= sendResponse
                SnapshotPackagesR -> getSnapshotPackagesR slug >>= sendResponse
                DocsR -> getDocsR slug >>= sendResponse
        _ -> notFound
