module Handler.StackageIndex where

import Import
import Data.BlobStore

getStackageIndexR :: PackageSetIdent -> Handler TypedContent
getStackageIndexR ident = do
    msrc <- storeRead $ CabalIndex ident
    case msrc of
        Nothing -> notFound
        Just src -> do
            addHeader "content-disposition" "attachment; filename=\"00-index.tar.gz\""
            respondSource "application/x-gzip" $ mapOutput (Chunk . toBuilder) src
