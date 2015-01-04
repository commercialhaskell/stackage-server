-- | Code for unpacking documentation bundles, building the Hoogle databases,
-- and compressing/deduping contents.
module Data.Unpacking
    ( newDocUnpacker
    ) where

import Import hiding (runDB)
import Data.BlobStore
import Handler.Haddock
import Filesystem (createTree, isFile, removeTree, isDirectory, listDirectory)
import Control.Concurrent (forkIO)
import Control.Monad.Trans.Resource (allocate, resourceForkIO, release)
import Data.Char (isAlpha)
import qualified Hoogle
import qualified Data.Text as T
import qualified Data.Yaml as Y
import System.IO (IOMode (ReadMode, WriteMode), withBinaryFile, openBinaryFile)
import System.IO.Temp (withSystemTempFile, withTempFile, createTempDirectory, withSystemTempDirectory)
import System.Directory (getTemporaryDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (createProcess, proc, cwd, waitForProcess)

newDocUnpacker
    :: FilePath -- ^ haddock root
    -> BlobStore StoreKey
    -> (forall a m. (MonadIO m, MonadBaseControl IO m) => SqlPersistT m a -> m a)
    -> (Route App -> [(Text, Text)] -> Text)
    -> IO DocUnpacker
newDocUnpacker root store runDB urlRender = do
    createDirs dirs

    statusMapVar <- newTVarIO $ asMap mempty
    messageVar <- newTVarIO "Inactive"
    workChan <- atomically newTChan

    let requestDocs forceUnpack ent = atomically $ do
            var <- newTVar USBusy
            modifyTVar statusMapVar
                       $ insertMap (stackageSlug $ entityVal ent) var
            writeTChan workChan (forceUnpack, ent, var)

    forkForever $ unpackWorker dirs runDB store statusMapVar messageVar urlRender workChan

    return DocUnpacker
        { duRequestDocs = \ent -> do
            m <- readTVarIO statusMapVar
            case lookup (stackageSlug $ entityVal ent) m of
                Nothing -> do
                    b <- isUnpacked dirs ent
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
isUnpacked :: Dirs -> Entity Stackage -> IO Bool
isUnpacked dirs (Entity _ stackage) =
    isFile databasePath
  where
    databasePath = dirHoogleFp dirs (stackageIdent stackage) ["default.hoo"]

forkForever :: IO () -> IO ()
forkForever inner = mask $ \restore ->
    void $ forkIO $ forever $ handleAny print $ restore $ forever inner

unpackWorker dirs runDB store statusMapVar messageVar urlRender workChan = do
    atomically $ writeTVar messageVar "Waiting for new work item"
    (forceUnpack, ent, resVar) <- atomically $ readTChan workChan
    shouldUnpack <-
        if forceUnpack
            then return True
            else not <$> isUnpacked dirs ent
    when shouldUnpack $ do
        let say msg = atomically $ writeTVar messageVar $ concat
                [ toPathPiece (stackageSlug $ entityVal ent)
                , ": "
                , msg
                ]
        say "Beginning of processing"
        eres <- tryAny $ unpacker dirs runDB store say urlRender ent
        atomically $ writeTVar resVar $ case eres of
            Left e -> USFailed $ tshow e
            Right () -> USReady

removeTreeIfExists :: FilePath -> IO ()
removeTreeIfExists fp = whenM (isDirectory fp) (removeTree fp)

unpacker dirs runDB store say urlRender stackageEnt@(Entity _ Stackage {..}) = do
    say "Removing old directories, if they exist"
    removeTreeIfExists $ dirRawIdent dirs stackageIdent
    removeTreeIfExists $ dirGzIdent dirs stackageIdent
    removeTreeIfExists $ dirHoogleIdent dirs stackageIdent

    withSystemTempFile "haddock-bundle.tar.xz" $ \tempfp temph -> do
        say "Downloading raw tarball"
        withAcquire (storeRead' store (HaddockBundle stackageIdent)) $ \msrc ->
            case msrc of
                Nothing -> error "No haddocks exist for that snapshot"
                Just src -> src $$ sinkHandle temph
        hClose temph

        let destdir = dirRawIdent dirs stackageIdent
        createTree destdir
        say "Unpacking tarball"
        (Nothing, Nothing, Nothing, ph) <- createProcess
            (proc "tar" ["xf", tempfp])
                { cwd = Just $ fpToString destdir
                }
        ec <- waitForProcess ph
        if ec == ExitSuccess then return () else throwM ec

        createTree $ dirHoogleIdent dirs stackageIdent
        tmp <- getTemporaryDirectory

        -- Determine which packages have documentation and update the
        -- database appropriately
        say "Updating database for available documentation"
        runResourceT $ runDB $ do
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

        withSystemTempDirectory "hoogle-database-gen" $ \hoogletemp' -> do
            let hoogletemp = fpFromString hoogletemp'
                logFp = fpToString (dirHoogleFp dirs stackageIdent ["error-log"])
            withBinaryFile logFp WriteMode $ \errorLog -> do
                say "Copying Hoogle text files to temp directory"
                runResourceT $ copyHoogleTextFiles errorLog destdir hoogletemp
                say "Creating Hoogle database"
                createHoogleDb say dirs stackageEnt errorLog hoogletemp urlRender

copyHoogleTextFiles :: Handle -- ^ error log handle
                    -> FilePath -- ^ raw unpacked Haddock files
                    -> FilePath -- ^ temporary work directory
                    -> ResourceT IO ()
copyHoogleTextFiles errorLog raw tmp = do
    let tmptext = tmp </> "text"
    liftIO $ createTree tmptext
    sourceDirectory raw $$ mapM_C (\fp ->
        forM_ (nameAndVersionFromPath fp) $ \(name, version) -> do
            let src = fp </> fpFromText name <.> "txt"
                dst = tmptext </> fpFromText (name ++ "-" ++ version)
            exists <- liftIO $ isFile src
            if exists
                then sourceFile src $$ (sinkFile dst :: Sink ByteString (ResourceT IO) ())
                else liftIO $ appendHoogleErrors errorLog $ HoogleErrors
                    { packageName = name
                    , packageVersion = version
                    , errors = ["No textual Hoogle DB (use \"cabal haddock --hoogle\")"]
                    }
            )

createHoogleDb :: (Text -> IO ())
               -> Dirs
               -> Entity Stackage
               -> Handle -- ^ error log handle
               -> FilePath -- ^ temp directory
               -> (Route App -> [(Text, Text)] -> Text)
               -> IO ()
createHoogleDb say dirs (Entity _ stackage) errorLog tmpdir urlRender = do
    let ident = stackageIdent stackage
        tmpbin = tmpdir </> "binary"
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
                    appendHoogleErrors errorLog $ HoogleErrors
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
                (fpToString (dirHoogleFp dirs ident ["default.hoo"]))
    case eres of
        Right () -> return ()
        Left err -> liftIO $ appendHoogleErrors errorLog $ HoogleErrors
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
appendHoogleErrors :: Handle -> HoogleErrors -> IO ()
appendHoogleErrors h errs = hPut h (Y.encode [errs])

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
