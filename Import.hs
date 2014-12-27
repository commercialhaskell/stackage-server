module Import
    ( module Import
    ) where

import ClassyPrelude.Yesod as Import
import Foundation as Import
import Model as Import
import Settings as Import
import Settings.Development as Import
import Settings.StaticFiles as Import
import Types as Import
import Yesod.Auth as Import
import Data.Slug (mkSlug)
import Data.WebsiteContent as Import (WebsiteContent (..))
import Data.Text.Read (decimal)
import Data.Conduit.Zlib (ungzip)
import System.IO (openBinaryFile, IOMode (ReadMode))
import Data.Yaml (decodeEither')
import Control.Monad.Trans.Resource (allocate)
import Data.Slug (SnapSlug)

requireAuthIdOrToken :: Handler UserId
requireAuthIdOrToken = do
    mtoken <- lookupHeader "authorization"
    case decodeUtf8 <$> mtoken of
        Nothing -> requireAuthId
        Just token -> do
            case mkSlug token of
                Nothing -> invalidArgs ["Invalid token: " ++ token]
                Just token' -> do
                    muser <- runDB $ getBy $ UniqueToken token'
                    case muser of
                        Nothing -> invalidArgs ["Unknown token: " ++ token]
                        Just (Entity uid _) -> return uid

parseLtsPair :: Text -> Maybe (Int, Int)
parseLtsPair t1 = do
    (x, t2) <- either (const Nothing) Just $ decimal t1
    t3 <- stripPrefix "." t2
    (y, "") <- either (const Nothing) Just $ decimal t3
    Just (x, y)

getStackage :: SnapSlug -> Handler (Entity Stackage, Maybe SnapshotInfo)
getStackage slug = do
    ent@(Entity _ stackage) <- runDB $ getBy404 $ UniqueSnapshot slug
    msi <-
        if stackageYaml stackage
            then Just <$> getSnapshotInfoByIdent (stackageIdent stackage)
            else return Nothing
    return (ent, msi)

getSnapshotInfoByIdent :: PackageSetIdent -> Handler SnapshotInfo
getSnapshotInfoByIdent ident = withCache $ do
    dirs <- getDirs
    let sourceDocFile rest = do
            let rawfp = fpToString $ dirRawFp dirs ident rest
                gzfp  = fpToString $ dirGzFp dirs ident rest
            eres <- liftResourceT $ tryIO $ allocate (openBinaryFile rawfp ReadMode) hClose
            case eres of
                Left _ -> do
                    (_, h) <- allocate (openBinaryFile gzfp ReadMode) hClose
                    sourceHandle h $= ungzip
                Right (_, h) -> sourceHandle h

    let maxFileSize = 1024 * 1024 * 5
        yaml :: FromJSON a => Text -> Handler a
        yaml name = do
            bs <- sourceDocFile [name] $$ takeCE maxFileSize =$ foldC
            either throwM return $ decodeEither' bs

    master <- getYesod
    liftIO $ haddockUnpacker master False ident

    siType <- yaml "build-type.yaml"
    siPlan <- yaml "build-plan.yaml"
    siDocMap <- yaml "docs-map.yaml"
    return SnapshotInfo {..}
  where
    withCache inner = do
        cacheRef <- snapshotInfoCache <$> getYesod
        cache <- readIORef cacheRef
        case lookup ident cache of
            Just x -> return x
            Nothing -> do
                x <- inner
                atomicModifyIORef' cacheRef $ \m ->
                    (insertMap ident x m, x)


data Dirs = Dirs
    { dirRawRoot :: !FilePath
    , dirGzRoot :: !FilePath
    , dirCacheRoot :: !FilePath
    }

getDirs :: Handler Dirs
getDirs = mkDirs . haddockRootDir <$> getYesod

mkDirs :: FilePath -> Dirs
mkDirs dir = Dirs
    { dirRawRoot = dir </> "idents-raw"
    , dirGzRoot = dir </> "idents-gz"
    , dirCacheRoot = dir </> "cachedir"
    }

dirGzIdent, dirRawIdent :: Dirs -> PackageSetIdent -> FilePath
dirGzIdent dirs ident = dirGzRoot dirs </> fpFromText (toPathPiece ident)
dirRawIdent dirs ident = dirRawRoot dirs </> fpFromText (toPathPiece ident)

dirGzFp, dirRawFp :: Dirs -> PackageSetIdent -> [Text] -> FilePath
dirGzFp dirs ident rest = dirGzIdent dirs ident </> mconcat (map fpFromText rest)
dirRawFp dirs ident rest = dirRawIdent dirs ident </> mconcat (map fpFromText rest)
