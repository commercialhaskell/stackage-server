module Handler.StackageIndex where

import Import
import Data.BlobStore
import Data.Slug (SnapSlug)

getStackageIndexR :: SnapSlug -> Handler TypedContent
getStackageIndexR slug = do
    error "getStackageIndexR"
    {-
    Entity _ stackage <- runDB $ getBy404 $ UniqueSnapshot slug
    let ident = stackageIdent stackage
    msrc <- storeRead $ CabalIndex ident
    case msrc of
        Nothing -> notFound
        Just src -> do
            setEtag $ toPathPiece ident
            addHeader "content-disposition" "attachment; filename=\"00-index.tar.gz\""
            neverExpires
            respondSource "application/x-gzip" $ mapOutput (Chunk . toBuilder) src
            -}
