module Handler.StackageSdist where

import Import
import Data.BlobStore
import Data.Hackage

getStackageSdistR :: PackageSetIdent -> PackageNameVersion -> Handler TypedContent
getStackageSdistR ident (PackageNameVersion name version) = do
    addDownload (Just ident) Nothing name version
    msrc1 <- storeRead (CustomSdist ident name version)
    msrc <-
        case msrc1 of
            Just src -> return $ Just src
            Nothing -> sourceHackageSdist name version
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

addDownload :: Maybe PackageSetIdent
            -> Maybe HackageView
            -> PackageName
            -> Version
            -> Handler ()
addDownload downloadIdent downloadView downloadPackage downloadVersion = do
    downloadUserAgent <- fmap decodeUtf8 <$> lookupHeader "user-agent"
    downloadTimestamp <- liftIO getCurrentTime
    runDB $ insert_ Download {..}
