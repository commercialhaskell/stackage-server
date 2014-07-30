module Handler.HackageViewSdist where

import Import
import Data.Hackage

getHackageViewSdistR :: HackageView -> PackageNameVersion -> Handler TypedContent
getHackageViewSdistR viewName (PackageNameVersion name version) = do
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
