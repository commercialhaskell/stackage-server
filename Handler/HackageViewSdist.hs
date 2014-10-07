module Handler.HackageViewSdist where

import Import
import Data.Hackage
import Handler.StackageSdist (addDownload)

getHackageViewSdistR :: HackageView -> PackageNameVersion -> Handler TypedContent
getHackageViewSdistR viewName (PackageNameVersion name version) = do
    addDownload Nothing (Just viewName) name version
    msrc <- sourceHackageViewSdist viewName name version
    case msrc of
        Nothing -> notFound
        Just src -> do
            addHeader "content-disposition" $ concat
                [ "attachment; filename=\""
                , toPathPiece name
                , "-"
                , toPathPiece version
                , ".tar.gz"
                ]
            respondSource "application/x-gzip" $ mapOutput (Chunk . toBuilder) src
