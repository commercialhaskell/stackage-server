module Handler.Haddock where

import Import
import Data.BlobStore
import Filesystem (removeTree, isDirectory, createTree, isFile, rename, removeFile)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import System.IO.Temp (withSystemTempFile, withTempFile)
import Control.Exception (mask)
import System.Process (createProcess, proc, cwd, waitForProcess)
import System.Exit (ExitCode (ExitSuccess))
import Network.Mime (defaultMimeLookup)
import Crypto.Hash.Conduit (sinkHash)
import System.IO (IOMode (ReadMode), withBinaryFile)
import Data.Conduit.Zlib (gzip)
import System.Posix.Files (createLink)
import qualified Data.ByteString.Base16 as B16
import Data.Byteable (toBytes)
import Crypto.Hash (Digest, SHA1)

form :: Form FileInfo
form = renderDivs $ areq fileField "tarball containing docs"
    { fsName = Just "tarball"
    } Nothing

getUploadHaddockR, putUploadHaddockR :: PackageSetIdent -> Handler Html
getUploadHaddockR ident = do
    uid <- requireAuthIdOrToken
    Entity sid Stackage {..} <- runDB $ getBy404 $ UniqueStackage ident
    unless (uid == stackageUser) $ permissionDenied "You do not control this snapshot"
    ((res, widget), enctype) <- runFormPostNoToken form
    case res of
        FormSuccess fileInfo -> do
            fileSource fileInfo $$ storeWrite (HaddockBundle ident)
            runDB $ update sid [StackageHasHaddocks =. True]
            master <- getYesod
            getHaddockDir ident >>= liftIO . void . tryIO . removeTree
            void $ liftIO $ forkIO $ haddockUnpacker master ident
            setMessage "Haddocks uploaded"
            redirect $ StackageHomeR ident
        _ -> defaultLayout $ do
            setTitle "Upload Haddocks"
            $(widgetFile "upload-haddock")

putUploadHaddockR = getUploadHaddockR

getHaddockR :: PackageSetIdent -> [Text] -> Handler ()
getHaddockR ident rest = do
    sanitize $ toPathPiece ident
    mapM_ sanitize rest
    dir <- getHaddockDir ident
    master <- getYesod
    liftIO $ unlessM (isDirectory dir) $ haddockUnpacker master ident
    let fp = mconcat $ dir : map fpFromText rest

    whenM (liftIO $ isDirectory fp)
        $ redirect $ HaddockR ident $ rest ++ ["index.html"]

    let fpgz = fp <.> "gz"
        mime = defaultMimeLookup $ fpToText $ filename fp
    whenM (liftIO $ isFile fpgz) $ do
        addHeader "Content-Encoding" "gzip"
        sendFile mime $ fpToString fpgz

    whenM (liftIO $ isFile fp) $ sendFile mime $ fpToString fp

    notFound
  where
    sanitize p
        | ("/" `isInfixOf` p) || p `member` (asHashSet $ setFromList ["", ".", ".."]) =
            permissionDenied "Invalid request"
        | otherwise = return ()

createHaddockUnpacker :: FilePath -- ^ haddock root
                      -> BlobStore StoreKey
                      -> IO (PackageSetIdent -> IO ())
createHaddockUnpacker root store = do
    chan <- newChan

    mask $ \restore -> void $ forkIO $ forever $ do
        (ident, res) <- readChan chan
        try (restore $ go ident) >>= putMVar res
        restore (gzipHash ident) `catch` (print . asSomeException)
    return $ \ident -> do
        res <- newEmptyMVar
        writeChan chan (ident, res)
        takeMVar res >>= either (throwM . asSomeException) return
  where
    go ident = unlessM (isDirectory dir) $
               withSystemTempFile "haddock-bundle.tar.xz" $ \tempfp temph -> do
        withAcquire (storeRead' store (HaddockBundle ident)) $ \msrc ->
            case msrc of
                Nothing -> error "No haddocks exist for that snapshot"
                Just src -> src $$ sinkHandle temph
        hClose temph
        createTree dir
        (Nothing, Nothing, Nothing, ph) <- createProcess
            (proc "tar" ["xf", tempfp])
                { cwd = Just $ fpToString dir
                }
        ec <- waitForProcess ph
        if ec == ExitSuccess then return () else throwM ec
      where
        dir = mkDir ident

    mkDir ident = root </> "idents" </> fpFromText (toPathPiece ident)

    -- Procedure is to:
    --
    -- * Traverse the entire directory
    -- * Gzip each file to a temp file, and get a hash of the contents
    -- * If that hash doesn't exist in the cache, move the new file to the cache
    -- * Create a hard link from /orig/file.gz to the file in the cache
    -- * Delete /orig/file
    gzipHash ident = do
        createTree cachedir
        runResourceT $ sourceDirectoryDeep False dir
                    $$ mapM_C (liftIO . handle (print . asIOException) . oneFile)
      where
        dir = mkDir ident
        cachedir = root </> "cache-dir"

        oneFile fp = withTempFile (fpToString cachedir) "haddock-file.gz" $ \tempfp temph -> do
            digest <- withBinaryFile (fpToString fp) ReadMode $ \inh ->
                sourceHandle inh
                $= gzip
                $$ (getZipSink $
                    ZipSink (sinkHandle temph) *>
                    ZipSink sinkHash)
            hClose temph
            let name = decodeUtf8 $ B16.encode $ toBytes (digest :: Digest SHA1)
            let fpcache = cachedir </> fpFromText name <.> "gz"
            unlessM (isFile fpcache) $ rename (fpFromString tempfp) fpcache
            createLink (fpToString fpcache) (fpToString $ fp <.> "gz")
            removeFile fp
