module Handler.Haddock
    ( getUploadHaddockR
    , putUploadHaddockR
    , getHaddockR
    , getUploadDocMapR
    , putUploadDocMapR
    , createHaddockUnpacker
    -- Exported for use in Handler.Hoogle
    , Dirs, getDirs, dirHoogleFp
    ) where

import Import
import Data.BlobStore
import Filesystem (removeTree, isDirectory, createTree, isFile, rename, removeFile, removeDirectory, listDirectory)
import System.Directory (getTemporaryDirectory)
import Control.Concurrent (forkIO)
import System.IO.Temp (withSystemTempFile, withTempFile, createTempDirectory)
import System.Process (createProcess, proc, cwd, waitForProcess)
import System.Exit (ExitCode (ExitSuccess))
import Network.Mime (defaultMimeLookup)
import Crypto.Hash.Conduit (sinkHash)
import System.IO (IOMode (ReadMode), withBinaryFile, openBinaryFile)
import Data.Conduit.Zlib (gzip)
import System.Posix.Files (createLink)
import qualified Data.ByteString.Base16 as B16
import Data.Byteable (toBytes)
import Crypto.Hash (Digest, SHA1)
import qualified Filesystem.Path.CurrentOS as F
import Data.Slug (SnapSlug, unSlug)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Data.Aeson (withObject)
import qualified Hoogle
import Data.Char (isAlpha)
import Control.Monad.Trans.Resource (allocate, resourceForkIO, release)

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
            void $ liftIO $ forkIO $ haddockUnpacker master True stackageEnt
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
    master <- getYesod
    liftIO $ haddockUnpacker master False stackageEnt

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

-- Should have two threads: one to unpack, one to convert. Never serve the
-- uncompressed files, only the compressed files. When serving, convert on
-- demand.
createHaddockUnpacker :: FilePath -- ^ haddock root
                      -> BlobStore StoreKey
                      -> (forall a m. (MonadIO m, MonadBaseControl IO m)
                            => SqlPersistT m a -> m a)
                      -> IORef (Route App -> [(Text, Text)] -> Text)
                      -> IO (IORef Text, ForceUnpack -> Entity Stackage -> IO ())
createHaddockUnpacker root store runDB' urlRenderRef = do
    createTree $ dirCacheRoot dirs
    createTree $ dirRawRoot dirs
    createTree $ dirGzRoot dirs
    createTree $ dirHoogleRoot dirs

    chan <- newChan
    (statusRef, compressor) <- createCompressor dirs

    mask $ \restore -> void $ forkIO $ forever $ do
        (forceUnpack, ident, res) <- readChan chan
        try (restore $ go forceUnpack ident) >>= putMVar res
        compressor
    return (statusRef, \forceUnpack stackageEnt -> do
        let ident = stackageIdent (entityVal stackageEnt)
        shouldAct <-
            if forceUnpack
                then return True
                else not <$> doDirsExist ident
        if shouldAct
            then do
                res <- newEmptyMVar
                writeChan chan (forceUnpack, stackageEnt, res)
                takeMVar res >>= either (throwM . asSomeException) return
            else return ())
  where
    dirs = mkDirs root

    removeTreeIfExists fp = whenM (isDirectory fp) (removeTree fp)

    doDirsExist ident = do
        e1 <- isDirectory $ dirGzIdent dirs ident
        if e1
            then return True
            else isDirectory $ dirRawIdent dirs ident
    go forceUnpack stackageEnt = do
        let ident = stackageIdent (entityVal stackageEnt)
        toRun <-
            if forceUnpack
                then do
                    removeTreeIfExists $ dirRawIdent dirs ident
                    removeTreeIfExists $ dirGzIdent dirs ident
                    removeTreeIfExists $ dirHoogleIdent dirs ident
                    return True
                else not <$> doDirsExist ident
        when toRun $ do
            withSystemTempFile "haddock-bundle.tar.xz" $ \tempfp temph -> do
                withAcquire (storeRead' store (HaddockBundle ident)) $ \msrc ->
                    case msrc of
                        Nothing -> error "No haddocks exist for that snapshot"
                        Just src -> src $$ sinkHandle temph
                hClose temph
                let destdir = dirRawIdent dirs ident
                createTree destdir
                (Nothing, Nothing, Nothing, ph) <- createProcess
                    (proc "tar" ["xf", tempfp])
                        { cwd = Just $ fpToString destdir
                        }
                ec <- waitForProcess ph
                if ec == ExitSuccess then return () else throwM ec

                -- TODO: run hoogle and the database update in
                -- concurrent threads.

                urlRender <- readIORef urlRenderRef
                runResourceT $ do
                    tmp <- liftIO getTemporaryDirectory
                    (_releasekey, hoogletemp) <- allocate
                        (fpFromString <$> createTempDirectory tmp "hoogle-database-gen")
                        removeTree
                    copyHoogleTextFiles destdir hoogletemp
                    void $ resourceForkIO $ createHoogleDb dirs stackageEnt hoogletemp urlRender

                -- Determine which packages have documentation and update the
                -- database appropriately
                runResourceT $ runDB' $ do
                    let sid = entityKey stackageEnt
                    updateWhere
                        [PackageStackage ==. sid]
                        [PackageHasHaddocks =. False]
                    sourceDirectory destdir $$ mapM_C (\fp -> do
                        let mnv = nameAndVersionFromPath fp
                        forM_ mnv $ \(name, version) -> updateWhere
                            [ PackageStackage ==. sid
                            , PackageName' ==. PackageName name
                            , PackageVersion ==. Version version
                            ]
                            [PackageHasHaddocks =. True]
                        )

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

copyHoogleTextFiles :: FilePath -- ^ raw unpacked Haddock files
                    -> FilePath -- ^ temporary work directory
                    -> ResourceT IO ()
copyHoogleTextFiles raw tmp = do
    let tmptext = tmp </> "text"
    liftIO $ createTree tmptext
    sourceDirectory raw $$ mapM_C (\fp ->
        forM_ (nameAndVersionFromPath fp) $ \(name, version) -> do
            let src = fp </> fpFromText name <.> "txt"
                dst = tmptext </> fpFromText (name ++ "-" ++ version)
            whenM (liftIO $ isFile src) $
                sourceFile src $$ (sinkFile dst :: Sink ByteString (ResourceT IO) ())
            )

createHoogleDb :: Dirs
               -> Entity Stackage
               -> FilePath -- ^ temp directory
               -> (Route App -> [(Text, Text)] -> Text)
               -> ResourceT IO ()
createHoogleDb dirs (Entity _ stackage) tmpdir urlRender = do
    let ident = stackageIdent stackage
        tmpbin = tmpdir </> "binary"
        hoogleDir = dirHoogleIdent dirs ident
    liftIO $ do
        createTree hoogleDir
        createTree tmpbin
    -- Create hoogle binary databases for each package
    sourceDirectory (tmpdir </> "text") $$ mapM_C
      ( \fp -> do
        (releaseKey, srcH) <- allocate (openBinaryFile (fpToString fp) ReadMode) hClose
        forM_ (nameAndVersionFromPath fp) $ \(name, version) -> liftIO $ do
            src <- unpack . decodeUtf8 . asLByteString <$> hGetContents srcH
            let -- Preprocess the haddock-generated manifest file.
                src' = unlines $ haddockHacks (Just (unpack docsUrl)) $ lines src
                docsUrl = urlRender (HaddockR (stackageSlug stackage) urlPieces) []
                urlPieces = [name <> "-" <> version, "index.html"]
                -- Compute the filepath of the resulting hoogle
                -- database.
                out = fpToString $ tmpbin </> base <.> "hoo"
                base = F.dropExtension $ filename fp
            errs <- Hoogle.createDatabase "foo" Hoogle.Haskell [] src' out
            -- TODO: handle these more gracefully?
            when (not $ null errs) $ putStrLn $ concat
                [ fpToText base
                , " Hoogle errors: "
                , tshow errs
                ]
        release releaseKey
        )
    -- Merge the individual binary databases into one big database.
    liftIO $ do
        dbs <- listDirectory tmpbin
        let merged = hoogleDir </> "default.hoo"
        Hoogle.mergeDatabase
            (map fpToString (filter (/= merged) dbs))
            (fpToString merged)

nameAndVersionFromPath :: FilePath -> Maybe (Text, Text)
nameAndVersionFromPath fp =
    (\name -> (name, version)) <$> stripSuffix "-" name'
  where
    (name', version) = T.breakOnEnd "-" $ fpToText $ filename fp

---------------------------------------------------------------------
-- HADDOCK HACKS
-- (Copied from hoogle-4.2.36/src/Recipe/Haddock.hs)
-- Modifications:
-- 1) Some name qualification
-- 2) Explicit type sig due to polymorphic elem
-- 3) Fixed an unused binding warning

-- Eliminate @version
-- Change :*: to (:*:), Haddock bug
-- Change !!Int to !Int, Haddock bug
-- Change instance [overlap ok] to instance, Haddock bug
-- Change instance [incoherent] to instance, Haddock bug
-- Change instance [safe] to instance, Haddock bug
-- Change !Int to Int, HSE bug
-- Drop {-# UNPACK #-}, Haddock bug
-- Drop everything after where, Haddock bug

haddockHacks :: Maybe Hoogle.URL -> [String] -> [String]
haddockHacks loc src = maybe id haddockPackageUrl loc (translate src)
    where
        translate :: [String] -> [String]
        translate = map (unwords . g . map f . words) . filter (not . isPrefixOf "@version ")

        f "::" = "::"
        f (':':xs) = "(:" ++ xs ++ ")"
        f ('!':'!':x:xs) | isAlpha x = xs
        f ('!':x:xs) | isAlpha x || x `elem` ("[(" :: String) = x:xs
        f x | x `elem` ["[overlap","ok]","[incoherent]","[safe]"] = ""
        f x | x `elem` ["{-#","UNPACK","#-}"] = ""
        f x = x

        g ("where":_) = []
        g (x:xs) = x : g xs
        g [] = []

haddockPackageUrl :: Hoogle.URL -> [String] -> [String]
haddockPackageUrl x = concatMap f
    where f y | "@package " `isPrefixOf` y = ["@url " ++ x, y]
              | otherwise = [y]
