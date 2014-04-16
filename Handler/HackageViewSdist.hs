module Handler.HackageViewSdist where

import Import
import Data.BlobStore
import Data.Hackage
import Data.Conduit.Lazy (MonadActive (..))

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

instance MonadActive m => MonadActive (HandlerT site m) where -- FIXME upstream
    monadActive = lift monadActive
