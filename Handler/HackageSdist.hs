module Handler.HackageSdist where

import Import
import Data.Hackage

getHackageSdistR :: PackageName -> Version -> Handler TypedContent
getHackageSdistR name version = do
    msrc <- sourceHackageSdist name version
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
