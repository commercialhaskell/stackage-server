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
import qualified Filesystem.Path.CurrentOS as F

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
            void $ liftIO $ forkIO $ haddockUnpacker master True ident
            setMessage "Haddocks uploaded"
            redirect $ StackageHomeR ident
        _ -> defaultLayout $ do
            setTitle "Upload Haddocks"
            $(widgetFile "upload-haddock")

putUploadHaddockR = getUploadHaddockR

getHaddockR :: PackageSetIdent -> [Text] -> Handler ()
getHaddockR ident rest = do
    error "getHaddockR"
    {-
    sanitize $ toPathPiece ident
    mapM_ sanitize rest
    (gzdir, rawdir) <- getHaddockDir ident
    master <- getYesod
    liftIO $ unlessM (isDirectory dir) $ haddockUnpacker master ident

    let rawfp = mconcat $ rawdir : map fpFromText rest
        gzfp  = mconcat $ gzdir  : map fpFromText rest
        mime = defaultMimeLookup $ fpToText $ filename rawfp

    whenM (liftIO $ isDirectory rawfp)
        $ redirect $ HaddockR ident $ rest ++ ["index.html"]
    whenM (liftIO $ isDirectory gzfp)
        $ redirect $ HaddockR ident $ rest ++ ["index.html"]

    whenM (liftIO $ isFile gzfp) $ do
        addHeader "Content-Encoding" "gzip"
        sendFile mime $ fpToString gzfp

    whenM (liftIO $ isFile rawfp) $ sendFile mime $ fpToString rawfp

    notFound
  where
    sanitize p
        | ("/" `isInfixOf` p) || p `member` (asHashSet $ setFromList ["", ".", ".."]) =
            permissionDenied "Invalid request"
        | otherwise = return ()
        -}

getHaddockDir :: PackageSetIdent -> Handler (FilePath, FilePath)
getHaddockDir ident = do
    master <- getYesod
    return $ mkDirPair (haddockRootDir master) ident

mkDirPair :: FilePath -- ^ root
          -> PackageSetIdent
          -> (FilePath, FilePath) -- ^ compressed, uncompressed
mkDirPair root ident =
    ( root </> "idents-raw" </> fpFromText (toPathPiece ident)
    , root </> "idents-gz"  </> fpFromText (toPathPiece ident)
    )

createCompressor
    :: FilePath -- ^ uncompressed dir
    -> FilePath -- ^ compressed dir
    -> FilePath -- ^ cache dir
    -> IO (IO ()) -- ^ action to kick off compressor again
createCompressor rawdir gzdir cachedir = do
    baton <- newMVar ()
    mask $ \restore -> void $ forkIO $ forever $ do
        takeMVar baton
        runResourceT $ sourceDirectoryDeep False rawdir $$ mapM_C go
    return $ void $ tryPutMVar baton ()
  where
    go fp = liftIO $ handle (print . asSomeException) $ do
        gzipHash cachedir fp (gzdir </> suffix)
      where
        Just suffix = F.stripPrefix (rawdir </> "") fp

-- Procedure is to:
--
-- * Gzip the src file to a temp file, and get a hash of the gzipped contents
-- * If that hash doesn't exist in the cache, move the new file to the cache
-- * Create a hard link from dst to the file in the cache
-- * Delete src
gzipHash :: FilePath -- ^ cache directory
         -> FilePath -- ^ source
         -> FilePath -- ^ destination
         -> IO ()
gzipHash cachedir src dst = do
    withTempFile (fpToString cachedir) "haddock-file.gz" $ \tempfp temph -> do
        digest <- withBinaryFile (fpToString src) ReadMode $ \inh ->
            sourceHandle inh
            $= gzip
            $$ (getZipSink $
                ZipSink (sinkHandle temph) *>
                ZipSink sinkHash)
        hClose temph
        let name = decodeUtf8 $ B16.encode $ toBytes (digest :: Digest SHA1)
        let fpcache = cachedir </> fpFromText name <.> "gz"
        unlessM (isFile fpcache) $ rename (fpFromString tempfp) fpcache
        createLink (fpToString fpcache) (fpToString dst)
        removeFile src


-- Should have two threads: one to unpack, one to convert. Never serve the
-- uncompressed files, only the compressed files. When serving, convert on
-- demand.
createHaddockUnpacker :: FilePath -- ^ haddock root
                      -> BlobStore StoreKey
                      -> IO (ForceUnpack -> PackageSetIdent -> IO ())
createHaddockUnpacker root store = do
    chan <- newChan
    compressor <- createCompressor
        (root </> "idents-raw")
        (root </> "idents-gz")
        cacehdir

    mask $ \restore -> void $ forkIO $ forever $ do
        (forceUnpack, ident, res) <- readChan chan
        try (restore $ go forceUnpack ident) >>= putMVar res
        compressor
    return $ \forceUnpack ident -> do
        res <- newEmptyMVar
        writeChan chan (forceUnpack, ident, res)
        takeMVar res >>= either (throwM . asSomeException) return
  where
    cacehdir = root </> "cachedir"
    gzipHash = error "gzipHash"
    go forceUnpack ident = do
        error "go"
    {- FIXME
            unlessM (isDirectory dir) $
            withSystemTempFile "haddock-bundle.tar.xz" $ \tempfp temph -> do
        when forceUnpack
            $ liftIO $ mapM_ (void . tryIO . removeTree) [dir1, dir2]
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
    -}
