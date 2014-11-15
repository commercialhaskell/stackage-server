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
            cacheSeconds 31536000
            respondSource "application/x-gzip" $ mapOutput (Chunk . toBuilder) src

-- FIXME BEGIN: move into yesod-core

-- | Send a 304 not modified response immediately. This is a short-circuiting
-- action.
notModified :: MonadHandler m => m a
notModified = sendWaiResponse $ responseBuilder status304 [] mempty

-- | Check the if-none-match header and, if it matches the given value, return
-- a 304 not modified response. Otherwise, set the etag header to the given
-- value.
setEtag :: MonadHandler m => Text -> m ()
setEtag etag = do
    mmatch <- lookupHeader "if-none-match"
    case mmatch of
        Just x | encodeUtf8 etag == x -> notModified
        _ -> addHeader "etag" etag

-- FIXME END:   move into yesod-core

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
