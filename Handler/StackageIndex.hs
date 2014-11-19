module Handler.StackageIndex where

import Import
import Data.BlobStore
import Network.Wai (responseBuilder)

getStackageIndexR :: PackageSetIdent -> Handler TypedContent
getStackageIndexR ident = do
    msrc <- storeRead $ CabalIndex ident
    case msrc of
        Nothing -> notFound
        Just src -> do
            setEtag $ toPathPiece ident
            addHeader "content-disposition" "attachment; filename=\"00-index.tar.gz\""
            neverExpires
            respondSource "application/x-gzip" $ mapOutput (Chunk . toBuilder) src

getStackageBundleR :: PackageSetIdent -> Handler TypedContent
getStackageBundleR ident = do
    msrc <- storeRead $ SnapshotBundle ident
    case msrc of
        Nothing -> notFound
        Just src -> do
            addHeader "content-disposition" $ mconcat
                [ "attachment; filename=\"bundle-"
                , toPathPiece ident
                , ".tar.gz\""
                ]
            respondSource "application/x-gzip" $ mapOutput (Chunk . toBuilder) src
