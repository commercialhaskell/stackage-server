-- | Code for unpacking documentation bundles, building the Hoogle databases,
-- and compressing/deduping contents.
module Data.Unpacking
    ( newDocUnpacker
    , getHoogleDB
    , makeHoogle
    , createHoogleDatabases
    ) where

import Import hiding (runDB)
import Data.BlobStore
import Handler.Haddock
import Filesystem (createTree, isFile, removeTree, isDirectory, listDirectory, removeDirectory, removeFile, rename)
import System.Posix.Files (createLink)
import Crypto.Hash.Conduit (sinkHash)
import Control.Concurrent (forkIO)
import Control.Monad.Trans.Resource (allocate, release)
import Data.Char (isAlpha)
import qualified Hoogle
import qualified Data.Text as T
import qualified Data.Yaml as Y
import System.IO (IOMode (ReadMode), withBinaryFile, openBinaryFile)
import System.IO.Temp (withSystemTempFile, withTempFile, withSystemTempDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (createProcess, proc, cwd, waitForProcess)
import qualified Filesystem.Path.CurrentOS as F
import Data.Conduit.Zlib (gzip, ungzip)
import qualified Data.ByteString.Base16 as B16
import Data.Byteable (toBytes)
import Crypto.Hash (Digest, SHA1)

newDocUnpacker
    :: FilePath -- ^ haddock root
    -> BlobStore StoreKey
    -> (forall a m. (MonadIO m, MonadBaseControl IO m) => SqlPersistT m a -> m a)
    -> IO DocUnpacker
newDocUnpacker root store runDB = do
    createDirs dirs

    statusMapVar <- newTVarIO $ asMap mempty
    messageVar <- newTVarIO "Inactive"
    workChan <- atomically newTChan

    let requestDocs forceUnpack ent = atomically $ do
            var <- newTVar USBusy
            modifyTVar statusMapVar
                       $ insertMap (stackageSlug $ entityVal ent) var
            writeTChan workChan (forceUnpack, ent, var)

    forkForever $ unpackWorker dirs runDB store messageVar workChan

    return DocUnpacker
        { duRequestDocs = \ent -> do
            m <- readTVarIO statusMapVar
            case lookup (stackageSlug $ entityVal ent) m of
                Nothing -> do
                    b <- isUnpacked dirs (entityVal ent)
                    if b
                        then return USReady
                        else do
                            requestDocs False ent
                            return USBusy
                Just us -> readTVarIO us
        , duGetStatus = readTVarIO messageVar
        , duForceReload = \ent -> do
            atomically $ modifyTVar statusMapVar
                       $ deleteMap (stackageSlug $ entityVal ent)
            requestDocs True ent
        }
  where
    dirs = mkDirs root

createDirs :: Dirs -> IO ()
createDirs dirs = do
    createTree $ dirCacheRoot dirs
    createTree $ dirRawRoot dirs
    createTree $ dirGzRoot dirs
    createTree $ dirHoogleRoot dirs

-- | Check for the presence of file system artifacts indicating that the docs
-- have been unpacked.
isUnpacked :: Dirs -> Stackage -> IO Bool
isUnpacked dirs stackage = isFile $ completeUnpackFile dirs stackage

defaultHooDest :: Dirs -> Stackage -> FilePath
defaultHooDest dirs stackage = dirHoogleFp dirs (stackageIdent stackage)
    ["default-" ++ VERSION_hoogle ++ ".hoo"]

forkForever :: IO () -> IO ()
forkForever inner = mask $ \restore ->
    void $ forkIO $ forever $ handleAny print $ restore $ forever inner

unpackWorker
    :: Dirs
    -> (forall a m. (MonadIO m, MonadBaseControl IO m) => SqlPersistT m a -> m a)
    -> BlobStore StoreKey
    -> TVar Text
    -> TChan (Bool, Entity Stackage, TVar UnpackStatus)
    -> IO ()
unpackWorker dirs runDB store messageVar workChan = do
    atomically $ writeTVar messageVar "Waiting for new work item"
    (forceUnpack, ent, resVar) <- atomically $ readTChan workChan
    shouldUnpack <-
        if forceUnpack
            then return True
            else not <$> isUnpacked dirs (entityVal ent)

    let say msg = atomically $ writeTVar messageVar $ concat
            [ toPathPiece (stackageSlug $ entityVal ent)
            , ": "
            , msg
            ]

    when shouldUnpack $ do
        say "Beginning of processing"

        -- As soon as the raw unpack is complete, start serving docs
        let onRawComplete = atomically $ writeTVar resVar USReady

        eres <- tryAny $ unpacker dirs runDB store say onRawComplete ent
        atomically $ writeTVar resVar $ case eres of
            Left e -> USFailed $ tshow e
            Right () -> USReady

    say "Running the compressor"
    let shouldStop = fmap not $ atomically $ isEmptyTChan workChan
    runCompressor shouldStop say dirs

removeTreeIfExists :: FilePath -> IO ()
removeTreeIfExists fp = whenM (isDirectory fp) (removeTree fp)

unpackRawDocsTo
    :: BlobStore StoreKey
    -> PackageSetIdent
    -> (Text -> IO ())
    -> FilePath
    -> IO ()
unpackRawDocsTo store ident say destdir =
    withSystemTempFile "haddock-bundle.tar.xz" $ \tempfp temph -> do
        say "Downloading raw doc tarball"
        withAcquire (storeRead' store (HaddockBundle ident)) $ \msrc ->
            case msrc of
                Nothing -> error "No haddocks exist for that snapshot"
                Just src -> src $$ sinkHandle temph
        hClose temph

        createTree destdir
        say "Unpacking tarball"
        (Nothing, Nothing, Nothing, ph) <- createProcess
            (proc "tar" ["xf", tempfp])
                { cwd = Just $ fpToString destdir
                }
        ec <- waitForProcess ph
        if ec == ExitSuccess then return () else throwM ec


unpacker
    :: Dirs
    -> (forall a m. (MonadIO m, MonadBaseControl IO m) => SqlPersistT m a -> m a)
    -> BlobStore StoreKey
    -> (Text -> IO ())
    -> IO () -- ^ onRawComplete
    -> Entity Stackage
    -> IO ()
unpacker dirs runDB store say onRawComplete (Entity sid Stackage {..}) = do
    say "Removing old directories, if they exist"
    removeTreeIfExists $ dirRawIdent dirs stackageIdent
    removeTreeIfExists $ dirGzIdent dirs stackageIdent
    removeTreeIfExists $ dirHoogleIdent dirs stackageIdent

    let destdir = dirRawIdent dirs stackageIdent
    unpackRawDocsTo store stackageIdent say destdir
    onRawComplete

    createTree $ dirHoogleIdent dirs stackageIdent

    -- Determine which packages have documentation and update the
    -- database appropriately
    say "Updating database for available documentation"
    runResourceT $ runDB $ do
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

    say "Unpack complete"
    writeFile "completeUnpackFile dirs ent" ("Complete" :: ByteString)

completeUnpackFile :: Dirs -> Stackage -> FilePath
completeUnpackFile dirs stackage =
    dirGzIdent dirs (stackageIdent stackage) </> "unpack-complete"

-- | Get the path to the Hoogle database, downloading from persistent storage
-- if necessary. This function will /not/ generate a new database, and
-- therefore is safe to run on a live web server.
getHoogleDB :: Dirs
            -> Stackage
            -> Handler (Maybe FilePath)
getHoogleDB dirs stackage = do
    exists <- liftIO $ isFile fp
    if exists
        then return $ Just fp
        else do
            msrc <- storeRead key
            case msrc of
                Nothing -> return Nothing
                Just src -> do
                    liftIO $ createTree $ F.parent fp
                    let tmpfp = fp <.> "tmp" -- FIXME add something random
                    src $$ ungzip =$ sinkFile tmpfp
                    liftIO $ rename tmpfp fp
                    return $ Just fp
  where
    fp = defaultHooDest dirs stackage
    key = HoogleDB (stackageIdent stackage) $ HoogleVersion VERSION_hoogle

-- | Make sure that the last 5 LTS and last 5 Nightly releases all have Hoogle
-- databases available.
createHoogleDatabases
    :: BlobStore StoreKey
    -> (forall a m. (MonadIO m, MonadBaseControl IO m) => SqlPersistT m a -> m a)
    -> (Text -> IO ())
    -> (Route App -> [(Text, Text)] -> Text)
    -> IO ()
createHoogleDatabases store runDB say urlRender = do
    stackages <- runDB $ do
        sids <- (++)
            <$> fmap (map $ ltsStackage . entityVal)
                    (selectList [] [Desc LtsMajor, Desc LtsMinor, LimitTo 5])
            <*> fmap (map $ nightlyStackage . entityVal)
                    (selectList [] [Desc NightlyDay, LimitTo 5])
        catMaybes <$> mapM get sids
    forM_ stackages $ \stackage -> do
        let say' x = say $ concat
                [ toPathPiece $ stackageSlug stackage
                , ": "
                , x
                ]
        handleAny (say' . tshow) $ makeHoogle store say' urlRender stackage

-- | Either download the Hoogle database from persistent storage, or create it.
makeHoogle
    :: BlobStore StoreKey
    -> (Text -> IO ())
    -> (Route App -> [(Text, Text)] -> Text)
    -> Stackage
    -> IO ()
makeHoogle store say urlRender stackage = do
    say "Making hoogle database"
    exists <- storeExists' store hoogleKey
    if exists
        then say "Hoogle database already exists, skipping"
        else do
            say "Generating Hoogle database"
            generate
  where
    ident = stackageIdent stackage
    hoogleKey = HoogleDB ident $ HoogleVersion VERSION_hoogle

    generate = withSystemTempDirectory "hoogle-database-gen" $ \hoogletemp' -> do
        let hoogletemp = fpFromString hoogletemp'
            rawdocs = hoogletemp </> "rawdocs"

        unpackRawDocsTo store ident say rawdocs

        say "Copying Hoogle text files to temp directory"
        runResourceT $ copyHoogleTextFiles say rawdocs hoogletemp
        say "Creating Hoogle database"
        withSystemTempFile "default.hoo" $ \dstFP' dstH -> do
            let dstFP = fpFromString dstFP'
            hClose dstH
            createHoogleDb say dstFP stackage hoogletemp urlRender
            say "Uploading database to persistent storage"
            withAcquire (storeWrite' store hoogleKey) $ \sink ->
                runResourceT $ sourceFile dstFP $$ gzip =$ sink

runCompressor :: IO Bool -- ^ should stop early?
              -> (Text -> IO ()) -> Dirs -> IO ()
runCompressor shouldStop say dirs =
    handle (\EarlyStop -> return ()) $ runResourceT $ goDir $ dirRawRoot dirs
  where
    goDir dir = do
        liftIO $ whenM shouldStop $ do
            say "Stopping compressor early"
            throwIO EarlyStop
        liftIO $ say $ "Compressing directory: " ++ fpToText dir
        sourceDirectory dir $$ mapM_C goFP
        liftIO $ void $ tryIO $ removeDirectory dir

    goFP fp = do
        e <- liftIO $ isFile fp
        if e
            then liftIO $ do
                liftIO $ say $ "Compressing file: " ++ fpToText fp
                handle (print . asSomeException)
                    $ gzipHash dirs suffix
            else goDir fp
      where
        Just suffix = F.stripPrefix (dirRawRoot dirs </> "") fp

data EarlyStop = EarlyStop
    deriving (Show, Typeable)
instance Exception EarlyStop

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

dirCacheFp :: Dirs -> Digest SHA1 -> FilePath
dirCacheFp dirs digest =
    dirCacheRoot dirs </> fpFromText x </> fpFromText y <.> "gz"
  where
    name = decodeUtf8 $ B16.encode $ toBytes digest
    (x, y) = splitAt 2 name

copyHoogleTextFiles :: (Text -> IO ()) -- ^ log
                    -> FilePath -- ^ raw unpacked Haddock files
                    -> FilePath -- ^ temporary work directory
                    -> ResourceT IO ()
copyHoogleTextFiles say raw tmp = do
    let tmptext = tmp </> "text"
    liftIO $ createTree tmptext
    sourceDirectory raw $$ mapM_C (\fp ->
        forM_ (nameAndVersionFromPath fp) $ \(name, version) -> do
            let src = fp </> fpFromText name <.> "txt"
                dst = tmptext </> fpFromText (name ++ "-" ++ version)
            exists <- liftIO $ isFile src
            if exists
                then sourceFile src $$ (sinkFile dst :: Sink ByteString (ResourceT IO) ())
                else liftIO $ appendHoogleErrors say $ HoogleErrors
                    { packageName = name
                    , packageVersion = version
                    , errors = ["No textual Hoogle DB (use \"cabal haddock --hoogle\")"]
                    }
            )

createHoogleDb :: (Text -> IO ())
               -> FilePath -- ^ default.hoo output location
               -> Stackage
               -> FilePath -- ^ temp directory
               -> (Route App -> [(Text, Text)] -> Text)
               -> IO ()
createHoogleDb say dstDefaultHoo stackage tmpdir urlRender = do
    let tmpbin = tmpdir </> "binary"
    createTree tmpbin
    eres <- tryAny $ runResourceT $ do
        -- Create hoogle binary databases for each package.
        sourceDirectory (tmpdir </> "text") $$ mapM_C
          ( \fp -> do
            (releaseKey, srcH) <- allocate (openBinaryFile (fpToString fp) ReadMode) hClose
            forM_ (nameAndVersionFromPath fp) $ \(name, version) -> liftIO $ do
                say $ concat
                    [ "Creating Hoogle database for: "
                    , name
                    , "-"
                    , version
                    ]
                src <- unpack . decodeUtf8 . asLByteString <$> hGetContents srcH
                let -- Preprocess the haddock-generated manifest file.
                    src' = unlines $ haddockHacks (Just (unpack docsUrl)) $ lines src
                    docsUrl = urlRender (HaddockR (stackageSlug stackage) urlPieces) []
                    urlPieces = [name <> "-" <> version, "index.html"]
                    -- Compute the filepath of the resulting hoogle
                    -- database.
                    out = fpToString $ tmpbin </> fpFromText base
                    base = name <> "-" <> version <> ".hoo"
                errs <- Hoogle.createDatabase "" Hoogle.Haskell [] src' out
                when (not $ null errs) $ do
                    -- TODO: remove this printing once errors are yielded
                    -- to the user.
                    putStrLn $ concat
                        [ base
                        , " Hoogle errors: "
                        , tshow errs
                        ]
                    appendHoogleErrors say $ HoogleErrors
                        { packageName = name
                        , packageVersion = version
                        , errors = map show errs
                        }
            release releaseKey
            )
        -- Merge the individual binary databases into one big database.
        liftIO $ do
            say "Merging all Hoogle databases"
            dbs <- listDirectory tmpbin
            Hoogle.mergeDatabase
                (map fpToString dbs)
                (fpToString dstDefaultHoo)
    case eres of
        Right () -> return ()
        Left err -> liftIO $ appendHoogleErrors say $ HoogleErrors
             { packageName = "Exception thrown while building hoogle DB"
             , packageVersion = ""
             , errors = [show err]
             }

data HoogleErrors = HoogleErrors
    { packageName :: Text
    , packageVersion :: Text
    , errors :: [String]
    } deriving (Generic)

instance ToJSON HoogleErrors where
instance FromJSON HoogleErrors where

-- Appends hoogle errors to a log file.  By encoding within a single
-- list, the resulting file can be decoded as [HoogleErrors].
appendHoogleErrors :: (Text -> IO ()) -> HoogleErrors -> IO ()
appendHoogleErrors say errs = say $ decodeUtf8 $ Y.encode [errs]

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
