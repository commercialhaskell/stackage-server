module Handler.HackageViewIndex where

import Import
import Data.BlobStore

getHackageViewIndexR :: HackageView -> Handler TypedContent
getHackageViewIndexR viewName = do
    msrc <- storeRead $ HackageViewIndex viewName
    case msrc of
        Nothing -> notFound
        Just src -> do
            addHeader "content-disposition" "attachment; filename=\"00-index.tar.gz\""
            respondSource "application/x-gzip" $ mapOutput (Chunk . toBuilder) src
