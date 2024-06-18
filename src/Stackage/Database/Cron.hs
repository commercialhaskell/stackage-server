{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Stackage.Database.Cron
    ( stackageServerCron
    , newHoogleLocker
    , singleRun
    , StackageCronOptions(..)
    , defHaddockBucketName
    , defHaddockBucketUrl
    ) where

import Conduit
import Control.DeepSeq
import Control.SingleRun
import Control.Lens ((?~))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Tar (FileInfo(..), FileType(..), untar)
import Data.Conduit.Zlib (WindowBits(WindowBits), compress, ungzip)
import qualified Data.IntMap.Strict as IntMap
import Data.Monoid (Any(..))
import Data.Streaming.Network (bindPortTCP)
import Data.Yaml (decodeFileEither)
import Database.Persist hiding (exists)
import Database.Persist.Postgresql hiding (exists)
import qualified Hoogle
import Amazonka hiding (Request, length, error)
import Amazonka.Data.Text (toText)
import Amazonka.S3
import Amazonka.S3.ListObjectsV2
import Amazonka.S3.Lens
import Amazonka.Lens
import Network.HTTP.Client
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Simple (getResponseBody, httpJSONEither)
import Network.HTTP.Types (status200, status404)
import Pantry (CabalFileInfo(..), DidUpdateOccur(..),
               HpackExecutable(HpackBundled), PackageIdentifierRevision(..),
               defaultCasaMaxPerRequest, defaultCasaRepoPrefix,
               defaultHackageSecurityConfig, defaultSnapshotLocation)
import Pantry.Internal.Stackage (HackageTarballResult(..), PantryConfig(..),
                                 Storage(..), forceUpdateHackageIndex,
                                 getHackageTarball, packageTreeKey)
import Path (parseAbsDir, toFilePath)
import RIO
import RIO.Directory
import RIO.File
import RIO.FilePath
import RIO.List as L
import qualified RIO.Map as Map
import RIO.Process (mkDefaultProcessContext)
import qualified RIO.Set as Set
import qualified RIO.Text as T
import RIO.Time
import Settings
import Stackage.Database.Github
import Stackage.Database.PackageInfo
import Stackage.Database.Query
import Stackage.Database.Schema
import Stackage.Database.Types
import System.Environment (getEnvironment)
import UnliftIO.Concurrent (getNumCapabilities)
import Web.PathPieces (fromPathPiece, toPathPiece)
import qualified Control.Retry as Retry


hoogleKey :: SnapName -> Text
hoogleKey name = T.concat
    [ "hoogle/"
    , toPathPiece name
    , "/"
    , VERSION_hoogle
    , ".hoo"
    ]

hoogleUrl :: SnapName -> Text -> Text
hoogleUrl n haddockBucketUrl = T.concat
    [ haddockBucketUrl
    , "/"
    , hoogleKey n
    ]


hackageDeprecatedUrl :: Request
hackageDeprecatedUrl = "https://hackage.haskell.org/packages/deprecated.json"

withStorage :: (Storage -> IO a) -> IO a
withStorage inner = do
    as <- getAppSettings
    withStackageDatabase False (appDatabase as) (\db -> inner (Storage (runDatabase db) id))

getStackageSnapshotsDir :: RIO StackageCron FilePath
getStackageSnapshotsDir = do
    cron <- ask
    cloneOrUpdate (scStackageRoot cron) (scSnapshotsRepo cron)


withResponseUnliftIO :: MonadUnliftIO m => Request -> Manager -> (Response BodyReader -> m b) -> m b
withResponseUnliftIO req man f = withRunInIO $ \ runInIO -> withResponse req man (runInIO . f)

-- | Returns an action that, under the SingleRun wrapper that ensures only one
-- thing at a time is writing the file in question, ensure that a Hoogle
-- database exists on the filesystem for the given SnapName. But only going so
-- far as downloading it from the haddock bucket. See 'createHoogleDB' for the
-- function that puts it there in the first place. If no db exists in the
-- bucket, the action will return 'Nothing'.
newHoogleLocker ::
       (HasLogFunc env, MonadIO m) => env -> Manager -> Text -> m (SingleRun SnapName (Maybe FilePath))
newHoogleLocker env man bucketUrl = mkSingleRun hoogleLocker
  where
    hoogleLocker :: MonadIO n => SnapName -> n (Maybe FilePath)
    hoogleLocker name =
        runRIO env $ do
            let fp = T.unpack $ hoogleKey name
            exists <- doesFileExist fp
            if exists
                then return $ Just fp
                else do
                    req' <- parseRequest $ T.unpack $ hoogleUrl name bucketUrl
                    let req = req' {decompress = const False}
                    withResponseUnliftIO req man $ \res ->
                        case responseStatus res of
                            status
                                | status == status200 -> do
                                    createDirectoryIfMissing True $ takeDirectory fp
                                    withBinaryFileDurableAtomic fp WriteMode $ \h ->
                                        runConduitRes $
                                        bodyReaderSource (responseBody res) .| ungzip .|
                                        sinkHandle h
                                    return $ Just fp
                                | status == status404 -> do
                                    logWarn $ "NotFound: " <> display (hoogleUrl name bucketUrl)
                                    return Nothing
                                | otherwise -> do
                                    body <- liftIO $ brConsume $ responseBody res
                                    logWarn $ "Unexpected status: " <> displayShow status
                                    mapM_ (logWarn . displayBytesUtf8) body
                                    return Nothing

getHackageDeprecations ::
       (HasLogFunc env, MonadReader env m, MonadIO m) => m [Deprecation]
getHackageDeprecations = do
    let policy = Retry.exponentialBackoff 50 <> Retry.limitRetries 5
    jsonResponseDeprecated <-
      liftIO $ Retry.recoverAll policy $ const $ httpJSONEither hackageDeprecatedUrl
    case getResponseBody jsonResponseDeprecated of
        Left err -> do
            logError $
                "There was an error parsing deprecated.json file: " <>
                fromString (displayException err)
            return []
        Right deprecated -> return deprecated


stackageServerCron :: StackageCronOptions -> IO ()
stackageServerCron StackageCronOptions {..} = do
    void $
        -- Hacky approach instead of PID files
        catchIO (bindPortTCP 17834 "127.0.0.1") $
        const $ throwString "Stackage Cron loader process already running, exiting."
    connectionCount <- getNumCapabilities
    withStorage $ \storage -> do
        lo <- logOptionsHandle stdout True
        stackageRootDir <- getAppUserDataDirectory "stackage"
        pantryRootDir <- parseAbsDir (stackageRootDir </> "pantry")
        createDirectoryIfMissing True (toFilePath pantryRootDir)
        updateRef <- newMVar True
        cabalImmutable <- newIORef Map.empty
        cabalMutable <- newIORef Map.empty
        gpdCache <- newIORef IntMap.empty
        defaultProcessContext <- mkDefaultProcessContext
        aws <- do
            aws' <- newEnv discover
            endpoint <- lookup "AWS_S3_ENDPOINT" <$> getEnvironment
            pure $ case endpoint of
                Nothing -> aws'
                Just ep -> configureService (setEndpoint True (BS8.pack ep) 443 Amazonka.S3.defaultService) aws'
        withLogFunc (setLogMinLevel scoLogLevel lo) $ \logFunc -> do
            let pantryConfig =
                    PantryConfig
                        { pcHackageSecurity = defaultHackageSecurityConfig
                        , pcHpackExecutable = HpackBundled
                        , pcRootDir = pantryRootDir
                        , pcStorage = storage
                        , pcUpdateRef = updateRef
                        , pcParsedCabalFilesRawImmutable = cabalImmutable
                        , pcParsedCabalFilesMutable = cabalMutable
                        , pcConnectionCount = connectionCount
                        , pcCasaRepoPrefix = defaultCasaRepoPrefix
                        , pcCasaMaxPerRequest = defaultCasaMaxPerRequest
                        , pcSnapshotLocation = defaultSnapshotLocation
                        }
            currentHoogleVersionId <- runRIO logFunc $ do
                runStackageMigrations' pantryConfig
                getCurrentHoogleVersionIdWithPantryConfig pantryConfig
            let stackage =
                    StackageCron
                        { scPantryConfig = pantryConfig
                        , scStackageRoot = stackageRootDir
                        , scProcessContext = defaultProcessContext
                        , scLogFunc = logFunc
                        , scForceFullUpdate = scoForceUpdate
                        , scCachedGPD = gpdCache
                        , scEnvAWS = aws
                        , scDownloadBucketName = scoDownloadBucketName
                        , scDownloadBucketUrl = scoDownloadBucketUrl
                        , scUploadBucketName = scoUploadBucketName
                        , scSnapshotsRepo = scoSnapshotsRepo
                        , scReportProgress = scoReportProgress
                        , scCacheCabalFiles = scoCacheCabalFiles
                        , scHoogleVersionId = currentHoogleVersionId
                        }
            runRIO stackage (runStackageUpdate scoDoNotUpload)


runStackageUpdate :: Bool -> RIO StackageCron ()
runStackageUpdate doNotUpload = do
    forceFullUpdate <- scForceFullUpdate <$> ask
    logInfo $ "Starting stackage-cron update" <> bool "" " with --force-update" forceFullUpdate
    runStackageMigrations
    didUpdate <- forceUpdateHackageIndex (Just "stackage-server cron job")
    case didUpdate of
        UpdateOccurred   -> logInfo "Updated hackage index"
        NoUpdateOccurred -> logInfo "No new packages in hackage index"
    logInfo "Getting deprecated info now"
    getHackageDeprecations >>= setDeprecations
    corePackageGetters <- makeCorePackageGetters
    runResourceT $
        join $
        -- @createOrUpdateSnapshot@ processes package N while processing docs for
        -- package N-1. This @pure ()@ is the "documentation processing action"
        -- for the -1'th package.
        runConduit $ sourceSnapshots .| foldMC (createOrUpdateSnapshot corePackageGetters) (pure ())
    unless doNotUpload uploadSnapshotsJSON
    buildAndUploadHoogleDB doNotUpload
    logInfo "Finished building and uploading Hoogle DBs"


-- | This will look at 'global-hints.yaml' and will create core package getters that are reused
-- later for adding those package to individual snapshots.
makeCorePackageGetters ::
       RIO StackageCron (Map CompilerP [CorePackageGetter])
makeCorePackageGetters = do
    rootDir <- scStackageRoot <$> ask
    contentDir <- getStackageContentDir rootDir
    backupCoreCabalFiles <- getBackupCoreCabalFiles rootDir
    liftIO (decodeFileEither (contentDir </> "stack" </> "global-hints.yaml")) >>= \case
        Right (hints :: Map CompilerP (Map PackageNameP VersionP)) ->
            Map.traverseWithKey
                (\compiler ->
                     fmap Map.elems .
                     Map.traverseMaybeWithKey (makeCorePackageGetter compiler backupCoreCabalFiles))
                hints
        Left exc -> do
            logError $
                "Error parsing 'global-hints.yaml' file: " <> fromString (displayException exc)
            return mempty

-- | Packages distributed with GHC aren't taken from Hackage like normal
-- packages. Release managers do upload them, however, so that their docs are
-- available.
--
-- Or at least, they should. The release process was fragile, and some packages
-- weren't uploaded. This mechanism gives us a chance to fill in missing
-- packages.
getBackupCoreCabalFiles ::
       FilePath
    -> RIO StackageCron (Map PackageIdentifierP (GenericPackageDescription, CabalFileIds))
getBackupCoreCabalFiles rootDir = do
    backupCoreCabalFilesDir <- getBackupCoreCabalFilesDir rootDir
    cabalFileNames <- getDirectoryContents backupCoreCabalFilesDir
    cabalFiles <-
        forM (filter (isExtensionOf ".cabal") cabalFileNames) $ \cabalFileName ->
            let pidTxt = T.pack (dropExtension (takeFileName cabalFileName))
             in case fromPathPiece pidTxt of
                    Nothing -> do
                        logError $ "Invalid package identifier: " <> fromString cabalFileName
                        pure Nothing
                    Just pid -> do
                        cabalBlob <- readFileBinary (backupCoreCabalFilesDir </> cabalFileName)
                        mCabalInfo <- run $ addCabalFile pid cabalBlob
                        pure ((,) pid <$> mCabalInfo)
    pure $ Map.fromList $ catMaybes cabalFiles

-- | Core package info rarely changes between the snapshots, therefore it would be wasteful to
-- load, parse and update all packages from gloabl-hints for each snapshot. Instead we produce
-- a memoized version that will do it once initially and then return information about a
-- package on subsequent invocations.
--
-- FIXME: The compiler argument is unused (and never has been). Should it be used?
makeCorePackageGetter ::
       CompilerP
    -> Map PackageIdentifierP (GenericPackageDescription, CabalFileIds)
    -> PackageNameP
    -> VersionP
    -> RIO StackageCron (Maybe CorePackageGetter)
makeCorePackageGetter _compiler fallbackCabalFileMap pname ver =
    run (getHackageCabalByRev0 pid) >>= \case
        Nothing -> do
            logWarn $
                "Core package from global-hints: '" <> display pid <> "' was not found in pantry."
            forM (Map.lookup pid fallbackCabalFileMap) $ \(gpd, cabalFileIds) -> do
                logInfo $
                    "Falling back on '" <> display pid <>
                    ".cabal' file from the commercialhaskell/core-cabal-files repo"
                pure $ pure (Left cabalFileIds, Nothing, pid, gpd)
        Just (hackageCabalId, blobId, _) -> do
            pkgInfoRef <- newIORef Nothing -- use for caching of pkgInfo
            let getCabalFileIdsTree gpd =
                    \case
                        Just tree -> pure $ Right tree
                        Nothing -> Left <$> getCabalFileIds blobId gpd
            let getMemoPackageInfo =
                    readIORef pkgInfoRef >>= \case
                        Just pkgInfo -> return pkgInfo
                        Nothing -> do
                            whenM (scReportProgress <$> ask) $
                                logSticky $ "Loading core package: " <> display pid
                            -- I have no idea what's happening here. I guess I
                            -- don't know what it means to "load" a package.
                            -- What is actually going on?
                            htr <- getHackageTarball pir Nothing
                            case htrFreshPackageInfo htr of
                                Just (gpd, treeId) -> do
                                    eTree <-
                                        run $ do
                                            mTree <- getEntity treeId
                                            getCabalFileIdsTree gpd mTree
                                    let pkgInfo = (eTree, Just hackageCabalId, pid, gpd)
                                    gpd `deepseq` writeIORef pkgInfoRef $ Just pkgInfo
                                    pure pkgInfo
                                Nothing -> do
                                    (gpd, eCabalTree) <-
                                        run $ do
                                            cabalBlob <- loadBlobById blobId
                                            let gpd = parseCabalBlob cabalBlob
                                            mTree <- getTreeForKey (packageTreeKey (htrPackage htr))
                                            (,) gpd <$> getCabalFileIdsTree gpd mTree
                                    let pkgInfo = (eCabalTree, Just hackageCabalId, pid, gpd)
                                    gpd `deepseq` writeIORef pkgInfoRef $ Just pkgInfo
                                    pure pkgInfo
            pure $ Just getMemoPackageInfo
  where
    pid = PackageIdentifierP pname ver
    pir =
        PackageIdentifierRevision (unPackageNameP pname) (unVersionP ver) (CFIRevision (Revision 0))


-- | Populates the database with information about a package?
--
-- Specifically, a pantry package is being added to a particular snapshot.
--
-- Extra information like compiler and flags are passed on in order to properly
-- figure out dependencies and modules.
--
-- TODO: for now it is only from hackage. PantryPackage needs an update to use other origins
addPantryPackage ::
       SnapshotId -> CompilerP -> Bool -> Map FlagNameP Bool -> PantryPackage -> RIO StackageCron Bool
addPantryPackage snapId compiler isHidden flags (PantryPackage pcabal pTreeKey) = do
    env <- ask
    let pkgDescCache = scCachedGPD env
        cacheP = scCacheCabalFiles env
    let blobKeyToInt = fromIntegral . unSqlBackendKey . unBlobKey
    let cachedPkgDesc cabalBlobId pkgDesc =
            pkgDesc `deepseq`
            atomicModifyIORef' pkgDescCache (\cacheMap -> (IntMap.insert cabalBlobId pkgDesc cacheMap, pkgDesc))
    let getPkgDesc cabalBlobId =
            \case
                Just pkgDesc | cacheP -> cachedPkgDesc (blobKeyToInt cabalBlobId) pkgDesc
                Just pkgDesc -> pure pkgDesc
                Nothing | cacheP -> do
                    cacheMap <- readIORef pkgDescCache
                    case IntMap.lookup (blobKeyToInt cabalBlobId) cacheMap of
                        Just pkgDesc -> pure pkgDesc
                        Nothing ->
                            loadBlobById cabalBlobId >>=
                            cachedPkgDesc (blobKeyToInt cabalBlobId) . parseCabalBlob
                Nothing -> parseCabalBlob <$> loadBlobById cabalBlobId
    let storeHackageSnapshotPackage hackageCabalId mTreeId mpkgDesc =
            getTreeForKey pTreeKey >>= \case
                -- error case #1
                Just (Entity treeId' _)
                    | Just treeId <- mTreeId
                    , treeId /= treeId' -> do
                        lift $ logError $ "Pantry Tree Key mismatch for: " <> display pcabal
                        pure False
                -- happy case
                Just pkgTree@(Entity _ Tree {treeCabal})
                    | Just cabalBlobId <- treeCabal -> do
                        pkgDesc <- getPkgDesc cabalBlobId mpkgDesc
                        addSnapshotPackage snapId compiler Hackage (Right pkgTree) (Just hackageCabalId) isHidden flags packageId pkgDesc
                        pure True
                -- error case #2
                _ -> do
                  lift $ logError $ "Pantry is missing the source tree for " <> display pcabal
                  pure False
    mHackageCabalInfo <- run $ getHackageCabalByKey packageId (pcCabalKey pcabal)
    case mHackageCabalInfo of
        Nothing -> do
          logError $ "Could not find the cabal file for: " <> display pcabal
          pure False
        Just (hackageCabalId, Nothing) -> do
            mHPI <-
                htrFreshPackageInfo <$>
                getHackageTarball (toPackageIdentifierRevision pcabal) (Just pTreeKey)
            run $
                case mHPI of
                    Just (pkgDesc, treeId) -> storeHackageSnapshotPackage hackageCabalId (Just treeId) (Just pkgDesc)
                    Nothing -> storeHackageSnapshotPackage hackageCabalId Nothing Nothing
        Just (hackageCabalId, mTreeId) -> run $ storeHackageSnapshotPackage hackageCabalId mTreeId Nothing
  where
    packageId = PackageIdentifierP (pcPackageName pcabal) (pcVersion pcabal)




-- | Download a list of available .html files from S3 bucket for a particular resolver and record
-- in the database which modules have documentation available for them.
checkForDocs :: SnapshotId -> SnapName -> ResourceT (RIO StackageCron) ()
checkForDocs snapshotId snapName = do
    bucketName <- lift (scDownloadBucketName <$> ask)
    env <- asks scEnvAWS
    -- it is faster to download all modules in this snapshot separately, rather
    -- than process them with a conduit all the way to the database.
    packageModules <-
        runConduit $
            paginate env (listSnapshotObjects bucketName)
            .| concatMapC (fromMaybe [] . (^. listObjectsV2Response_contents))
            .| mapC (\obj -> toText (obj ^. object_key))
            .| concatMapC (T.stripSuffix ".html" >=> T.stripPrefix prefix >=> pathToPackageModule)
            .| sinkList
    -- Cache SnapshotPackageId rather than look it up many times for each module in the package.
    sidsCacheRef <- newIORef Map.empty
    -- The other half of the cores are used in 'updateSnapshot'
    n <- max 1 . (`div` 2) <$> getNumCapabilities
    unexpectedPackages <- lift $ pooledMapConcurrentlyN n (markModule sidsCacheRef) packageModules
    forM_ (Set.fromList $ catMaybes unexpectedPackages) $ \pid ->
        lift $ logWarn $
            "Documentation found for package '" <> display pid <>
            "', which does not exist in this snapshot: " <>
        display snapName
  where
    prefix = textDisplay snapName <> "/"
    listSnapshotObjects bucketName = newListObjectsV2 (BucketName bucketName) & listObjectsV2_prefix ?~ prefix
    -- | This function records all package modules that have documentation available, the ones
    -- that are not found in the snapshot reported back as an error.  Besides being run
    -- concurrently this function optimizes the SnapshotPackageId lookup as well, since that can
    -- be shared amongst many modules of one package.
    markModule sidsCacheRef (pid, modName) = do
        sidsCache <- readIORef sidsCacheRef
        let mSnapshotPackageId = Map.lookup pid sidsCache
        mFound <- run $ markModuleHasDocs snapshotId pid mSnapshotPackageId modName
        case mFound of
            Nothing -> pure $ Just pid -- This package doesn't exist in the snapshot!
            Just snapshotPackageId
                | Nothing <- mSnapshotPackageId -> do
                    atomicModifyIORef'
                        sidsCacheRef
                        (\cacheMap -> (Map.insert pid snapshotPackageId cacheMap, ()))
                    pure Nothing
            _ -> pure Nothing

data SnapshotFileInfo = SnapshotFileInfo
    { sfiSnapName           :: !SnapName
    , sfiUpdatedOn          :: !UTCTime
    , sfiSnapshotFileGetter :: !(RIO StackageCron (Maybe SnapshotFile))
    }

-- | Use 'github.com/commercialhaskell/stackage-snapshots' repository to source all of the packages
-- one snapshot at a time.
sourceSnapshots :: ConduitT a SnapshotFileInfo (ResourceT (RIO StackageCron)) ()
sourceSnapshots = do
    snapshotsDir <- lift $ lift getStackageSnapshotsDir
    sourceDirectoryDeep False (snapshotsDir </> "lts") .| concatMapMC (getLtsParser snapshotsDir)
    sourceDirectoryDeep False (snapshotsDir </> "nightly") .| concatMapMC (getNightlyParser snapshotsDir)
  where
    makeSnapshotFileInfo gitDir fp mFileNameDate snapName = do
        let parseSnapshot updatedOn = do
                esnap <- liftIO $ decodeFileEither fp
                case esnap of
                    Right snap ->
                        let publishDate =
                                sfPublishDate snap <|> mFileNameDate <|> Just (utctDay updatedOn)
                         in return $ Just snap {sfPublishDate = publishDate}
                    Left exc -> do
                        logError $
                            "Error parsing snapshot file: " <> fromString fp <> "\n" <>
                            fromString (displayException exc)
                        return Nothing
        mUpdatedOn <- lastGitFileUpdate gitDir fp
        forM mUpdatedOn $ \updatedOn -> do
            env <- lift ask
            return $
                SnapshotFileInfo
                    { sfiSnapName = snapName
                    , sfiUpdatedOn = updatedOn
                    , sfiSnapshotFileGetter = runRIO env (parseSnapshot updatedOn)
                    }
    getLtsParser gitDir fp =
        case mapM (BS8.readInt . BS8.pack) $ take 2 $ reverse (splitPath fp) of
            Just [(minor, ".yaml"), (major, "/")] ->
                makeSnapshotFileInfo gitDir fp Nothing $ SNLts major minor
            _ -> do
                logError
                    ("Couldn't parse the filepath into an LTS version: " <> display (T.pack fp))
                return Nothing
    getNightlyParser gitDir fp =
        case mapM (BS8.readInt . BS8.pack) $ take 3 $ reverse (splitPath fp) of
            Just [(day, ".yaml"), (month, "/"), (year, "/")]
                | Just date <- fromGregorianValid (fromIntegral year) month day ->
                    makeSnapshotFileInfo gitDir fp (Just date) $ SNNightly date
            _ -> do
                logError
                    ("Couldn't parse the filepath into a Nightly date: " <> display (T.pack fp))
                return Nothing


data DecisionResult a e = NothingToDo | NoSnapshotFile | NeedsUpdate a e | DoesntExist e

-- | Creates a new `Snapshot` if it is not yet present in the database, and decides if update
-- is necessary when it already exists.
--
-- TODO: Silently ignoring snapshots where the getter returns Nothing seems like
-- a potential problem. Anyway I'd rather run it beforehand!
decideOnSnapshotUpdate :: SnapshotFileInfo -> RIO StackageCron (Maybe (SnapshotId, SnapshotFile))
decideOnSnapshotUpdate SnapshotFileInfo {sfiSnapName, sfiUpdatedOn, sfiSnapshotFileGetter} = do
    forceUpdate <- scForceFullUpdate <$> ask
    let mkLogMsg rest = "Snapshot with name: " <> display sfiSnapName <> " " <> rest
    mKeySnapFile <-
        run (getBy (UniqueSnapshot sfiSnapName)) >>= \case
            -- exists, up to date, no force-updated requested; nothing to do
            Just (Entity _key snap)
                | snapshotUpdatedOn snap == Just sfiUpdatedOn && not forceUpdate ->
                    return NothingToDo
            -- exists but updatedOn was not previously set.
            Just entity@(Entity _key snap)
                | Nothing <- snapshotUpdatedOn snap -> do
                    logWarn $ mkLogMsg "did not finish updating last time."
                    maybe NoSnapshotFile (NeedsUpdate entity) <$> sfiSnapshotFileGetter
            -- exists, but updatedOn does not match or force-update was requested.
            Just entity -> do
                unless forceUpdate $ logWarn $ mkLogMsg "was updated, applying new patch."
                maybe NoSnapshotFile (NeedsUpdate entity) <$> sfiSnapshotFileGetter
            -- does not exist
            Nothing -> maybe NoSnapshotFile DoesntExist <$> sfiSnapshotFileGetter
    -- Add new snapshot to the database, when necessary
    case mKeySnapFile of
        NothingToDo -> Nothing <$ logInfo (mkLogMsg "already exists and is up to date.")
        NoSnapshotFile -> Nothing <$ logWarn (mkLogMsg "has no (readable?) snapshot file.")
        NeedsUpdate (Entity oldSnapKey oldSnap) sf@SnapshotFile {sfCompiler, sfPublishDate}
            | Just publishDate <- sfPublishDate -> do
                let updatedSnap =
                        Snapshot sfiSnapName sfCompiler publishDate (snapshotUpdatedOn oldSnap)
                run $ replace oldSnapKey updatedSnap
                pure $ Just (oldSnapKey, sf)
            | otherwise -> return Nothing

        DoesntExist sf@SnapshotFile {sfCompiler, sfPublishDate}
            | Just publishDate <- sfPublishDate -> do
                logInfo $ mkLogMsg "is new, adding to the database."
                fmap (, sf) <$>
                    run (insertUnique (Snapshot sfiSnapName sfCompiler publishDate Nothing))
            | otherwise -> Nothing <$ logWarn (mkLogMsg "has no publish date, skipping.")

type CorePackageGetter
     = RIO StackageCron ( Either CabalFileIds (Entity Tree)
                        , Maybe HackageCabalId
                        , PackageIdentifierP
                        , GenericPackageDescription)

-- | This is an optimized version of snapshoat loading which can load a snapshot and documentation
-- info for previous snapshot at the same time. It will execute concurrently the loading of
-- current snapshot as well as an action that was passed as an argument. At the end it will return
-- an action that should be invoked in order to mark modules that have documentation available,
-- which in turn can be passed as an argument to the next snapshot loader.
-- Something something ouroboros.
--
-- Question: When do the docs for the last snapshot get loaded?
--
-- Well, this binding is called as @join $ runConduit $ foldMC (createOrUpdateSnapshot corePackageInfoGetters) (pure ())@
--
-- So the answer: the doc-loading action for the last snapshot gets returned by @runConduit $ foldMC ...@,
-- which means it gets executed by @join $ runConduit $ foldMC ...@.
--
-- Evidence:
--
-- Since @foldMC :: (a -> b -> m a) -> a -> ConduitT b o m a@, we see
--
-- @@
-- a ~ ResourceT (RIO Stackage Cron) () -- this is the doc-loading action
-- b ~ SnapshotFileInfo
-- m ~ ResourceT (RIO StackageCron)
-- @@

-- and the foldMC creates a @ConduitT SnapshotFileInfo o (ResourceT (RIO StackageCron)) (ResourceT (RIO StackageCron) ())@
--
-- TODO: It might be more efficient to just put all the actions (snapshot
-- creation and documentation writing both) on a queue and let a bunch of
-- workers chew on it. The current impl creates arbitrary synchronization points
-- with 'runConcurrently'. Granted, I don't know what a good chunk size would
-- actually be.
createOrUpdateSnapshot ::
       Map CompilerP [CorePackageGetter]
    -> ResourceT (RIO StackageCron) ()
    -> SnapshotFileInfo
    -> ResourceT (RIO StackageCron) (ResourceT (RIO StackageCron) ())
createOrUpdateSnapshot corePackageInfoGetters prevAction sfi@SnapshotFileInfo { sfiSnapName , sfiUpdatedOn } = do
    finishedDocs <- newIORef False
    runConcurrently
        (Concurrently (prevAction >> writeIORef finishedDocs True) *>
         Concurrently (lift (loadCurrentSnapshot finishedDocs)))
  where
    loadCurrentSnapshot finishedDocs = do
        loadDocs <-
            decideOnSnapshotUpdate sfi >>= \case
                -- Nothing to do, and thus no docs to process
                Nothing -> return $ pure ()
                Just (snapshotId, snapshotFile) ->
                    updateSnapshot
                        corePackageInfoGetters
                        snapshotId
                        sfiSnapName
                        sfiUpdatedOn
                        snapshotFile
        report <- scReportProgress <$> ask
        when report $
            unlessM (readIORef finishedDocs) $
            logSticky "Still loading the docs for previous snapshot ..."
        pure loadDocs

-- | Creates Lts or Nightly entity [Question(bryan): Why not do this when
-- creating the snapshot? Why is this a separate table anyway?] and updates all
-- packages in the snapshot. If any packages are missing they will be created.
-- Returns an action that will (a) check for available documentation for the
-- packages' modules and (b) mark the packages as documented when haddock is
-- present on AWS S3.
--
-- (Only after documentation has been checked will this snapshot be marked as
-- completely updated. This is required in case something goes wrong and process
-- is interrupted.)
updateSnapshot ::
       Map CompilerP [CorePackageGetter]
    -> SnapshotId
    -> SnapName
    -> UTCTime
    -> SnapshotFile
    -> RIO StackageCron (ResourceT (RIO StackageCron) ()) -- ^ Returns the action for processing docs
updateSnapshot corePackageGetters snapshotId snapName updatedOn SnapshotFile {..} = do
    insertSnapshotName snapshotId snapName
    loadedPackageCountRef <- newIORef (0 :: Int)
    let totalPackages = length sfPackages
        -- A wrapper for 'addPantryPackage' that extracts the package info from
        -- snapshot info, increments the count of loaded packages, and reports success
        -- as a Bool.
        addPantryPackageWithReport pp = do
            let PantryCabal {pcPackageName} = ppPantryCabal pp
                isHidden = fromMaybe False (Map.lookup pcPackageName sfHidden)
                flags = fromMaybe Map.empty $ Map.lookup pcPackageName sfFlags
            curSucc <- addPantryPackage snapshotId sfCompiler isHidden flags pp
            atomicModifyIORef' loadedPackageCountRef (\c -> (c + 1, ()))
            pure curSucc
    -- Leave some cores and db connections for the doc loader
    n <- max 1 . (`div` 2) <$> getNumCapabilities
    before <- getCurrentTime
    report <- scReportProgress <$> ask
    pantryUpdatesSucceeded <-
        runConcurrently
            (Concurrently
                 (when report (runProgressReporter loadedPackageCountRef totalPackages snapName)) *>
             Concurrently (pooledMapConcurrentlyN n addPantryPackageWithReport sfPackages))
    after <- getCurrentTime
    let timeTotal = round (diffUTCTime after before)
        (mins, secs) = timeTotal `quotRem` (60 :: Int)
        packagePerSecond = fromIntegral ((totalPackages * 100) `div` timeTotal) / 100 :: Float
        allPantryUpdatesSucceeded = and pantryUpdatesSucceeded
    logInfo $
        mconcat
            [ "Loading snapshot '"
            , display snapName
            , "' was done (in "
            , displayShow mins
            , "min "
            , displayShow secs
            , "sec). With average "
            , displayShow packagePerSecond
            , " packages/sec. There are still docs."
            ]
    case Map.lookup sfCompiler corePackageGetters of
        Nothing -> logError $ "Hints are not found for the compiler: " <> display sfCompiler
        Just _
            | not allPantryUpdatesSucceeded ->
                logWarn $
                mconcat
                    [ "There was an issue loading a snapshot '"
                    , display snapName
                    , "', deferring addition of packages "
                    , "from global-hints until next time."
                    ]
        Just compilerCorePackages ->
            forM_ compilerCorePackages $ \getCorePackageInfo -> do
                (eTree, mhcid, pid, gpd) <- getCorePackageInfo
                run $ addSnapshotPackage snapshotId sfCompiler Core eTree mhcid False mempty pid gpd
    return $ do
        checkForDocsSucceeded <-
            tryAny (checkForDocs snapshotId snapName) >>= \case
                Left exc -> do
                    logError $ "Received exception while getting the docs: " <> displayShow exc
                    return False
                Right () -> return True
        if allPantryUpdatesSucceeded &&
           checkForDocsSucceeded && Map.member sfCompiler corePackageGetters
            then do
                lift $ snapshotMarkUpdated snapshotId updatedOn
                logInfo $ "Created or updated snapshot '" <> display snapName <> "' successfully"
            else logError $ "There were errors while adding snapshot '" <> display snapName <> "'"


-- | Report how many packages has been loaded so far.
runProgressReporter :: IORef Int -> Int -> SnapName -> RIO StackageCron ()
runProgressReporter loadedPackageCountRef totalPackages snapName = do
    let reportProgress = do
            loadedPackageCount <- readIORef loadedPackageCountRef
            when (loadedPackageCount < totalPackages) $ do
                logSticky $
                    mconcat
                        [ "Loading snapshot '"
                        , display snapName
                        , "' ("
                        , displayShow loadedPackageCount
                        , "/"
                        , displayShow totalPackages
                        , ")"
                        ]
                threadDelay 1000000
                reportProgress
    reportProgress

-- | Uploads a json file to S3 with all latest snapshots per major lts version and one nightly.
uploadSnapshotsJSON :: RIO StackageCron ()
uploadSnapshotsJSON = do
    snapshots <- snapshotsJSON
    uploadBucket <- scUploadBucketName <$> ask
    let key = ObjectKey "snapshots.json"
    uploadFromRIO key $
        set putObject_acl (Just ObjectCannedACL_Public_read) $
        set putObject_contentType (Just "application/json") $
        newPutObject (BucketName uploadBucket) key (toBody snapshots)

-- | Writes a gzipped version of hoogle db into temporary file onto the file system and then uploads
-- it to S3. Temporary file is removed upon completion
uploadHoogleDB :: FilePath -> ObjectKey -> RIO StackageCron ()
uploadHoogleDB fp key =
    withTempFile (takeDirectory fp) (takeFileName fp <.> "gz") $ \fpgz h -> do
        runConduitRes $ sourceFile fp .| compress 9 (WindowBits 31) .| CB.sinkHandle h
        hClose h
        body <- toBody <$> readFileBinary fpgz
        uploadBucket <- scUploadBucketName <$> ask
        uploadFromRIO key $
            set putObject_acl (Just ObjectCannedACL_Public_read) $ newPutObject (BucketName uploadBucket) key body


uploadFromRIO :: (AWSRequest a, Typeable a,  Typeable (AWSResponse a)) => ObjectKey -> a -> RIO StackageCron ()
uploadFromRIO key po = do
    logInfo $ "Uploading " <> displayShow key <> " to S3 bucket."
    env <- asks scEnvAWS
    eres <- runResourceT $ trying _Error $ send env po
    case eres of
        Left e ->
            logError $ "Couldn't upload " <> displayShow key <> " to S3 becuase " <> displayShow e
        Right _ -> logInfo $ "Successfully uploaded " <> displayShow key <> " to S3"

buildAndUploadHoogleDB :: Bool -> RIO StackageCron ()
buildAndUploadHoogleDB doNotUpload = do
    snapshots <- lastLtsNightlyWithoutHoogleDb 5 5
    -- currentHoogleVersionId <- scHoogleVersionId <$> ask
    env <- ask
    awsEnv <- asks scEnvAWS
    bucketUrl <- asks scDownloadBucketUrl
    -- locker is an action that returns the path to a hoogle db, if one exists
    -- in the haddock bucket already.
    locker <- newHoogleLocker (env ^. logFuncL) (awsEnv ^. env_manager) bucketUrl
    let -- These bindings undo a questionable conflation of operations
        insertH = checkInsertSnapshotHoogleDb True
        checkH = checkInsertSnapshotHoogleDb False
    for_ snapshots $ \(snapshotId, snapName) ->
        -- Even though we just got a list of snapshots that don't have hoogle
        -- databases, we check again. For some reason. I don't see how this can
        -- actually be useful. both lastLtsNightlyWithoutHoogleDb and
        -- checkInsertSnapshotHoogleDb just check against SnapshotHoogleDb.
        -- Perhaps the check can be removed.
        unlessM (checkH snapshotId) $ do
            logInfo $ "Starting Hoogle database download: " <> display (hoogleKey snapName)
            mfp <- singleRun locker snapName
            case mfp of
                Just _ -> do
                    -- Something bad must have happened: we created the Hoogle db
                    -- previously, but didn't get to record it in our database.
                    logInfo $ "Current hoogle database exists for: " <> display snapName
                    void $ insertH snapshotId
                Nothing -> do
                    logInfo $ "Current hoogle database does not yet exist in the bucket for: " <> display snapName
                    mfp' <- createHoogleDB snapshotId snapName
                    forM_ mfp' $ \fp -> do
                        let key = hoogleKey snapName
                            dest = T.unpack key
                        createDirectoryIfMissing True $ takeDirectory dest
                        renamePath fp dest
                        unless doNotUpload $ do
                            uploadHoogleDB dest (ObjectKey key)
                            void $ insertH snapshotId

-- | Create a hoogle db from haddocks for the given snapshot, and upload it to
-- the haddock bucket.
createHoogleDB :: SnapshotId -> SnapName -> RIO StackageCron (Maybe FilePath)
createHoogleDB snapshotId snapName =
    -- FIXME: this handles *any* exception, which means it will swallow most
    -- signals
    handleAny logException $ do
        logInfo $ "Creating Hoogle DB for " <> display snapName
        downloadBucketUrl <- scDownloadBucketUrl <$> ask
        let root = "hoogle-gen"
            bindir = root </> "bindir"
            outname = root </> "output.hoo"
            tarKey = toPathPiece snapName <> "/hoogle/orig.tar"
            tarUrl = downloadBucketUrl <> "/" <> tarKey
            tarFP = root </> T.unpack tarKey
        -- When tarball is downloaded it is saved with durability and atomicity, so if it
        -- is present it is not in a corrupted state
        unlessM (doesFileExist tarFP) $ do
            req <- parseRequest $ T.unpack tarUrl
            env <- asks scEnvAWS
            let man = env ^. env_manager
            withResponseUnliftIO req {decompress = const True} man $ \res -> do
                throwErrorStatusCodes req res
                createDirectoryIfMissing True $ takeDirectory tarFP
                withBinaryFileDurableAtomic tarFP WriteMode $ \tarHandle ->
                    runConduitRes $ bodyReaderSource (responseBody res) .| sinkHandle tarHandle
        void $ tryIO $ removeDirectoryRecursive bindir
        void $ tryIO $ removeFile outname
        createDirectoryIfMissing True bindir
        withSystemTempDirectory ("hoogle-" ++ T.unpack (textDisplay snapName)) $ \tmpdir -> do
            Any hasRestored <-
                runConduitRes $
                sourceFile tarFP .|
                untar (restoreHoogleTxtFileWithCabal tmpdir snapshotId snapName) .|
                foldMapC Any
            unless hasRestored $ error "No Hoogle .txt files found"
            let args = ["generate", "--database=" ++ outname, "--local=" ++ tmpdir]
            logInfo $
                mconcat
                    [ "Merging databases... ("
                    , foldMap fromString $ L.intersperse " " ("hoogle" : args)
                    , ")"
                    ]
            liftIO $ Hoogle.hoogle args
            logInfo "Merge done"
            return $ Just outname
  where
    logException exc =
        logError ("Problem creating hoogle db for " <> display snapName <> ": " <> displayShow exc) $>
        Nothing


-- | Grabs hoogle txt file from the tarball and a matching cabal file from pantry.  Writes
-- them into supplied temp directory and yields the result of operation as a boolean for
-- every tar entry.
restoreHoogleTxtFileWithCabal ::
       FilePath
    -> SnapshotId
    -> SnapName
    -> FileInfo
    -> ConduitM ByteString Bool (ResourceT (RIO StackageCron)) ()
restoreHoogleTxtFileWithCabal tmpdir snapshotId snapName fileInfo =
    case fileType fileInfo of
        FTNormal -> do
            let txtFileName = T.decodeUtf8With T.lenientDecode $ filePath fileInfo
                txtPackageName = T.takeWhile (/= '.') txtFileName
                mpkg = fromPathPiece txtPackageName
            maybe (pure Nothing) (lift . lift . getSnapshotPackageCabalBlob snapshotId) mpkg >>= \case
                Nothing -> do
                    logWarn $
                        "Unexpected hoogle filename: " <> display txtFileName <>
                        " in orig.tar for snapshot: " <>
                        display snapName
                    yield False
                Just cabal -> do
                    writeFileBinary (tmpdir </> T.unpack txtPackageName <.> "cabal") cabal
                    sinkFile (tmpdir </> T.unpack txtFileName)
                    yield True
        _ -> yield False


pathToPackageModule
    :: Text
    -- ^ Input is like @ace-0.6/ACE-Combinators@
    -> Maybe (PackageIdentifierP, ModuleNameP)
pathToPackageModule txt =
    case T.split (== '/') txt of
        [pkgIdentifier, moduleNameDashes] -> do
             modName :: ModuleNameP <- fromPathPiece moduleNameDashes
             pkgId :: PackageIdentifierP <- fromPathPiece pkgIdentifier
             Just (pkgId, modName)
        _ -> Nothing
