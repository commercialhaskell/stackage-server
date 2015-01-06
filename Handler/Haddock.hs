module Handler.Haddock
    ( getUploadHaddockR
    , putUploadHaddockR
    , getHaddockR
    , getUploadDocMapR
    , putUploadDocMapR
    -- Exported for use in Handler.Hoogle
    , Dirs (..), getDirs, dirHoogleFp, mkDirs
    , dirRawIdent
    , dirGzIdent
    , dirHoogleIdent
    , createCompressor
    ) where

import           Control.Concurrent (forkIO)
import           Crypto.Hash (Digest, SHA1)
import           Crypto.Hash.Conduit (sinkHash)
import           Data.Aeson (withObject)
import           Data.BlobStore
import qualified Data.ByteString.Base16 as B16
import           Data.Byteable (toBytes)
import           Data.Conduit.Zlib (gzip)
import           Data.Slug (SnapSlug, unSlug)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import           Filesystem (isDirectory, createTree, isFile, rename, removeFile, removeDirectory)
import qualified Filesystem.Path.CurrentOS as F
import           Import
import           Network.Mime (defaultMimeLookup)
import           System.IO (IOMode (ReadMode), withBinaryFile)
import           System.IO.Temp (withTempFile)
import           System.Posix.Files (createLink)

form :: Form FileInfo
form = renderDivs $ areq fileField "tarball containing docs"
    { fsName = Just "tarball"
    } Nothing

getUploadHaddockR, putUploadHaddockR :: Text -> Handler Html
getUploadHaddockR slug0 = do
    uid <- requireAuthIdOrToken
    stackageEnt@(Entity sid Stackage {..}) <- runDB $ do
        -- Provide fallback for old URLs
        ment <- getBy $ UniqueStackage $ PackageSetIdent slug0
        case ment of
            Just ent -> return ent
            Nothing -> do
                slug <- maybe notFound return $ fromPathPiece slug0
                getBy404 $ UniqueSnapshot slug
    let ident = stackageIdent
        slug = stackageSlug
    unless (uid == stackageUser) $ permissionDenied "You do not control this snapshot"
    ((res, widget), enctype) <- runFormPostNoToken form
    case res of
        FormSuccess fileInfo -> do
            fileSource fileInfo $$ storeWrite (HaddockBundle ident)
            runDB $ update sid [StackageHasHaddocks =. True]
            master <- getYesod
            liftIO $ duForceReload (appDocUnpacker master) stackageEnt
            setMessage "Haddocks uploaded"
            redirect $ SnapshotR slug StackageHomeR
        _ -> defaultLayout $ do
            setTitle "Upload Haddocks"
            $(widgetFile "upload-haddock")

putUploadHaddockR = getUploadHaddockR

getHaddockR :: SnapSlug -> [Text] -> Handler ()
getHaddockR slug rest = do
    stackageEnt <- runDB $ do
        ment <- getBy $ UniqueSnapshot slug
        case ment of
            Just ent -> do
                case rest of
                    [pkgver] -> tryContentsRedirect ent pkgver
                    [pkgver, "index.html"] -> tryContentsRedirect ent pkgver
                    _ -> return ()
                return ent
            Nothing -> do
                Entity _ stackage <- getBy404
                                  $ UniqueStackage
                                  $ PackageSetIdent
                                  $ toPathPiece slug
                redirectWith status301 $ HaddockR (stackageSlug stackage) rest
    mapM_ sanitize rest
    dirs <- getDirs
    requireDocs stackageEnt

    let ident = stackageIdent (entityVal stackageEnt)
        rawfp = dirRawFp dirs ident rest
        gzfp  = dirGzFp dirs ident rest
        mime = defaultMimeLookup $ fpToText $ filename rawfp

    whenM (liftIO $ isDirectory rawfp)
        $ redirect $ HaddockR slug $ rest ++ ["index.html"]
    whenM (liftIO $ isDirectory gzfp)
        $ redirect $ HaddockR slug $ rest ++ ["index.html"]

    whenM (liftIO $ isFile gzfp) $ do
        addHeader "Content-Encoding" "gzip"
        sendFile mime $ fpToString gzfp

    -- Note: There's a small race window here, where the compressor thread
    -- could pull the rug out from under us. We can work around this by opening
    -- the file and, if that fails, try the compressed version again.
    whenM (liftIO $ isFile rawfp) $ sendFile mime $ fpToString rawfp

    notFound
  where
    sanitize p
        | ("/" `isInfixOf` p) || p `member` (asHashSet $ setFromList ["", ".", ".."]) =
            permissionDenied "Invalid request"
        | otherwise = return ()

-- | Try to redirect to the snapshot's package page instead of the
-- Haddock-generated HTML.
tryContentsRedirect :: Entity Stackage -> Text -> YesodDB App ()
tryContentsRedirect (Entity sid Stackage {..}) pkgver = do
    mdocs <- selectFirst
        [ DocsName ==. name
        , DocsVersion ==. version
        , DocsSnapshot ==. Just sid
        ]
        []
    forM_ mdocs $ const
         $ redirect
         $ SnapshotR stackageSlug
         $ StackageSdistR
         $ PNVNameVersion name version
  where
    (PackageName . dropDash -> name, Version -> version) = T.breakOnEnd "-" pkgver

dropDash :: Text -> Text
dropDash t = fromMaybe t $ stripSuffix "-" t

createCompressor
    :: Dirs
    -> IO (IORef Text, IO ()) -- ^ action to kick off compressor again
createCompressor dirs = do
    baton <- newMVar ()
    status <- newIORef "Compressor is idle"
    mask_ $ void $ forkIO $ (finallyE $ \e -> writeIORef status $ "Compressor thread exited: " ++ tshow e) $ forever $ do
        writeIORef status "Waiting for signal to start compressing"
        takeMVar baton
        writeIORef status "Received signal, traversing directories"
        let rawRoot = dirRawRoot dirs
        whenM (isDirectory rawRoot) $ runResourceT $ goDir status rawRoot
    return (status, void $ tryPutMVar baton ())
  where
    finallyE f g = mask $ \restore -> do
        restore g `catch` \e -> do
            () <- f $ Just (e :: SomeException)
            () <- throwIO e
            return ()
        f Nothing
    goDir status dir = do
        writeIORef status $ "Compressing directory: " ++ fpToText dir
        sourceDirectory dir $$ mapM_C (goFP status)
        liftIO $ void $ tryIO $ removeDirectory dir

    goFP status fp = do
        e <- liftIO $ isFile fp
        if e
            then liftIO $ do
                writeIORef status $ "Compressing file: " ++ fpToText fp
                handle (print . asSomeException)
                    $ gzipHash dirs suffix
            else goDir status fp
      where
        Just suffix = F.stripPrefix (dirRawRoot dirs </> "") fp

-- Procedure is to:
--
-- * Gzip the src file to a temp file, and get a hash of the gzipped contents
-- * If that hash doesn't exist in the cache, move the new file to the cache
-- * Create a hard link from dst to the file in the cache
-- * Delete src
gzipHash :: Dirs
         -> FilePath -- ^ suffix
         -> IO ()
gzipHash dirs suffix = do
    withTempFile (fpToString $ dirCacheRoot dirs) "haddock-file.gz" $ \tempfp temph -> do
        digest <- withBinaryFile (fpToString src) ReadMode $ \inh ->
            sourceHandle inh
            $= gzip
            $$ (getZipSink $
                ZipSink (sinkHandle temph) *>
                ZipSink sinkHash)
        hClose temph
        let fpcache = dirCacheFp dirs digest
        unlessM (isFile fpcache) $ do
            createTree $ F.parent fpcache
            rename (fpFromString tempfp) fpcache
        createTree $ F.parent dst
        createLink (fpToString fpcache) (fpToString dst)
        removeFile src
  where
    src = dirRawRoot dirs </> suffix
    dst = dirGzRoot dirs </> suffix

data Dirs = Dirs
    { dirRawRoot :: !FilePath
    , dirGzRoot :: !FilePath
    , dirCacheRoot :: !FilePath
    , dirHoogleRoot :: !FilePath
    }

getDirs :: Handler Dirs
getDirs = mkDirs . haddockRootDir <$> getYesod

mkDirs :: FilePath -> Dirs
mkDirs dir = Dirs
    { dirRawRoot = dir </> "idents-raw"
    , dirGzRoot = dir </> "idents-gz"
    , dirCacheRoot = dir </> "cachedir"
    , dirHoogleRoot = dir </> "hoogle"
    }

dirGzIdent, dirRawIdent, dirHoogleIdent :: Dirs -> PackageSetIdent -> FilePath
dirGzIdent dirs ident = dirGzRoot dirs </> fpFromText (toPathPiece ident)
dirRawIdent dirs ident = dirRawRoot dirs </> fpFromText (toPathPiece ident)
dirHoogleIdent dirs ident = dirHoogleRoot dirs </> fpFromText (toPathPiece ident)

dirGzFp, dirRawFp, dirHoogleFp :: Dirs -> PackageSetIdent -> [Text] -> FilePath
dirGzFp dirs ident rest = dirGzIdent dirs ident </> mconcat (map fpFromText rest)
dirRawFp dirs ident rest = dirRawIdent dirs ident </> mconcat (map fpFromText rest)
dirHoogleFp dirs ident rest = dirHoogleIdent dirs ident </> mconcat (map fpFromText rest)

dirCacheFp :: Dirs -> Digest SHA1 -> FilePath
dirCacheFp dirs digest =
    dirCacheRoot dirs </> fpFromText x </> fpFromText y <.> "gz"
  where
    name = decodeUtf8 $ B16.encode $ toBytes digest
    (x, y) = splitAt 2 name

data DocInfo = DocInfo Version (Map Text [Text])
instance FromJSON DocInfo where
    parseJSON = withObject "DocInfo" $ \o -> DocInfo
        <$> (Version <$> o .: "version")
        <*> o .: "modules"

getUploadDocMapR :: Handler Html
getUploadDocMapR = do
    uid <- requireAuthIdOrToken
    user <- runDB $ get404 uid
    extra <- getExtra
    when (unSlug (userHandle user) `notMember` adminUsers extra)
        $ permissionDenied "Must be an administrator"

    ((res, widget), enctype) <- runFormPostNoToken $ renderDivs $ (,)
        <$> areq
            fileField
            "YAML file with map" { fsName = Just "docmap" }
            Nothing
        <*> areq textField "Stackage ID" { fsName = Just "snapshot" } Nothing
    case res of
        FormSuccess (fi, snapshot) -> do
            Entity sid stackage <- runDB $ do
                ment <- getBy $ UniqueStackage $ PackageSetIdent snapshot
                case ment of
                    Just ent -> return ent
                    Nothing -> do
                        slug <- maybe notFound return $ fromPathPiece snapshot
                        getBy404 $ UniqueSnapshot slug
            unless (stackageHasHaddocks stackage) $ invalidArgs $ return
                "Cannot use a snapshot without docs for a docmap"
            bs <- fileSource fi $$ foldC
            case Y.decodeEither bs of
                Left e -> invalidArgs [pack e]
                Right m0 -> do
                    now <- liftIO getCurrentTime
                    render <- getUrlRender
                    runDB $ forM_ (mapToList $ asMap m0) $ \(package, DocInfo version ms) -> do
                        did <- insert Docs
                            { docsName = PackageName package
                            , docsVersion = version
                            , docsUploaded = now
                            , docsSnapshot = Just sid
                            }
                        forM_ (mapToList ms) $ \(name, pieces) -> do
                            let url = render $ HaddockR (stackageSlug stackage) pieces
                            insert_ $ Module did name url
                    setMessage "Doc map complete"
                    redirect UploadDocMapR
        _ -> defaultLayout $ do
            setTitle "Upload doc map"
            [whamlet|
                <form method=post action=?_method=PUT enctype=#{enctype}>
                    ^{widget}
                    <input type=submit .btn value="Set document map">
            |]

putUploadDocMapR :: Handler Html
putUploadDocMapR = getUploadDocMapR
