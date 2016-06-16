module Stackage.Database
    ( StackageDatabase
    , GetStackageDatabase (..)
    , SnapName (..)
    , SnapshotId ()
    , Snapshot (..)
    , newestSnapshot
    , newestLTS
    , newestLTSMajor
    , newestNightly
    , ltsMajorVersions
    , snapshotBefore
    , lookupSnapshot
    , snapshotTitle
    , PackageListingInfo (..)
    , getAllPackages
    , getPackages
    , getPackageVersionBySnapshot
    , createStackageDatabase
    , openStackageDatabase
    , ModuleListingInfo (..)
    , getSnapshotModules
    , getPackageModules
    , SnapshotPackage (..)
    , lookupSnapshotPackage
    , getDeprecated
    , LatestInfo (..)
    , getLatests
    , getDeps
    , getRevDeps
    , Package (..)
    , getPackage
    , prettyName
    , prettyNameShort
    , getSnapshotsForPackage
    , getSnapshots
    , countSnapshots
    , currentSchema
    , last5Lts5Nightly
    , snapshotsJSON
    , getPackageCount
    ) where

import Database.Sqlite (SqliteException)
import Web.PathPieces (toPathPiece)
import qualified Codec.Archive.Tar as Tar
import Database.Esqueleto.Internal.Language (From)
import Text.Markdown (markdown, msAddHeadingId, def)
import System.Directory (removeFile)
import Stackage.Database.Haddock
import System.FilePath (takeBaseName, takeExtension)
import ClassyPrelude.Conduit hiding (pi, FilePath, (</>))
import Text.Blaze.Html (Html, toHtml)
import Yesod.Form.Fields (Textarea (..))
import Stackage.Database.Types
import System.Directory (getAppUserDataDirectory)
import qualified Filesystem as F
import Filesystem.Path.CurrentOS (parent, filename, directory, FilePath, encodeString, (</>))
import Data.Conduit.Process
import Stackage.Types
import Stackage.Metadata
import Stackage.PackageIndex.Conduit
import Web.PathPieces (fromPathPiece)
import Data.Yaml (decodeFileEither)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.Logger
import System.IO.Temp
import qualified Database.Esqueleto as E
import Data.Yaml (decode)
import qualified Data.Aeson as A
import Types (SnapshotBranch(..))

currentSchema :: Int
currentSchema = 1

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Schema
    val Int
    deriving Show
Imported
    name SnapName
    type Text
    UniqueImported name type

Snapshot
    name SnapName
    ghc Text
    created Day
    UniqueSnapshot name
Lts
    snap SnapshotId
    major Int
    minor Int
    UniqueLts major minor
Nightly
    snap SnapshotId
    day Day
    UniqueNightly day
Package
    name Text
    latest Text
    synopsis Text
    homepage Text
    author Text
    maintainer Text
    licenseName Text
    description Html
    changelog Html
    UniquePackage name
SnapshotPackage
    snapshot SnapshotId
    package PackageId
    isCore Bool
    version Text
    UniqueSnapshotPackage snapshot package
Module
    package SnapshotPackageId
    name Text
    UniqueModule package name
Dep
    user PackageId
    uses Text -- avoid circular dependency issue when loading database
    range Text
    UniqueDep user uses
Deprecated
    package PackageId
    inFavorOf [PackageId]
    UniqueDeprecated package
|]

instance A.ToJSON Snapshot where
  toJSON Snapshot{..} =
    A.object [ "name"    A..= snapshotName
             , "ghc"     A..= snapshotGhc
             , "created" A..= formatTime defaultTimeLocale "%F" snapshotCreated
             ]

_hideUnusedWarnings
    :: ( SnapshotPackageId
       , SchemaId
       , ImportedId
       , LtsId
       , NightlyId
       , ModuleId
       , DepId
       , DeprecatedId
       ) -> ()
_hideUnusedWarnings _ = ()

newtype StackageDatabase = StackageDatabase ConnectionPool

class MonadIO m => GetStackageDatabase m where
    getStackageDatabase :: m StackageDatabase
instance MonadIO m => GetStackageDatabase (ReaderT StackageDatabase m) where
    getStackageDatabase = ask

sourcePackages :: MonadResource m => FilePath -> Producer m Tar.Entry
sourcePackages root = do
    dir <- liftIO $ cloneOrUpdate root "commercialhaskell" "all-cabal-metadata"
    bracketP
        (do
            (fp, h) <- openBinaryTempFile "/tmp" "all-cabal-metadata.tar"
            hClose h
            return fp)
        removeFile
        $ \fp -> do
            liftIO $ runIn dir "git" ["archive", "--output", fp, "--format", "tar", "master"]
            sourceTarFile False fp

sourceBuildPlans :: MonadResource m => FilePath -> Producer m (SnapName, FilePath, Either (IO BuildPlan) (IO DocMap))
sourceBuildPlans root = do
    forM_ ["lts-haskell", "stackage-nightly"] $ \repoName -> do
        dir <- liftIO $ cloneOrUpdate root "fpco" repoName
        sourceDirectory (encodeString dir) =$= concatMapMC (go Left . fromString)
        let docdir = dir </> "docs"
        whenM (liftIO $ F.isDirectory docdir) $
            sourceDirectory (encodeString docdir) =$= concatMapMC (go Right . fromString)
  where
    go wrapper fp | Just name <- nameFromFP fp = liftIO $ do
        let bp = decodeFileEither (encodeString fp) >>= either throwM return
        return $ Just (name, fp, wrapper bp)
    go _ _ = return Nothing

    nameFromFP fp = do
        base <- stripSuffix ".yaml" $ pack $ encodeString $ filename fp
        fromPathPiece base

cloneOrUpdate :: FilePath -> String -> String -> IO FilePath
cloneOrUpdate root org name = do
    exists <- F.isDirectory dest
    if exists
        then do
            let git = runIn dest "git"
            git ["fetch"]
            git ["reset", "--hard", "origin/master"]
        else runIn root "git" ["clone", url, name]
    return dest
  where
    url = "https://github.com/" ++ org ++ "/" ++ name ++ ".git"
    dest = root </> fromString name

runIn :: FilePath -> String -> [String] -> IO ()
runIn dir cmd args =
    withCheckedProcess cp $ \ClosedStream Inherited Inherited -> return ()
  where
    cp = (proc cmd args) { cwd = Just $ encodeString dir }

openStackageDatabase :: MonadIO m => FilePath -> m StackageDatabase
openStackageDatabase fp = liftIO $ do
    F.createTree $ parent fp
    fmap StackageDatabase $ runNoLoggingT $ createSqlitePool (pack $ encodeString fp) 7

getSchema :: FilePath -> IO (Maybe Int)
getSchema fp = do
    StackageDatabase pool <- openStackageDatabase fp
    eres <- try $ runSqlPool (selectList [] []) pool
    putStrLn $ "getSchema result: " ++ tshow eres
    case eres :: Either SqliteException [Entity Schema] of
        Right [Entity _ (Schema v)] -> return $ Just v
        _ -> return Nothing

createStackageDatabase :: MonadIO m => FilePath -> m ()
createStackageDatabase fp = liftIO $ do
    putStrLn "Entering createStackageDatabase"
    actualSchema <- getSchema fp
    let schemaMatch = actualSchema == Just currentSchema
    unless schemaMatch $ do
        putStrLn $ "Current schema does not match actual schema: " ++ tshow (actualSchema, currentSchema)
        putStrLn $ "Deleting " ++ pack (encodeString fp)
        void $ tryIO $ removeFile $ encodeString fp

    StackageDatabase pool <- openStackageDatabase fp
    flip runSqlPool pool $ do
        runMigration migrateAll
        unless schemaMatch $ insert_ $ Schema currentSchema

    root <- liftIO $ fmap (</> fromString "database") $ fmap fromString $ getAppUserDataDirectory "stackage"
    F.createTree root
    runResourceT $ do
        putStrLn "Updating all-cabal-metadata repo"
        flip runSqlPool pool $ sourcePackages root $$ getZipSink
            ( ZipSink (mapM_C addPackage)
           *> ZipSink (do
                deprs <- foldlC getDeprecated' []
                lift $ do
                    deleteWhere ([] :: [Filter Deprecated])
                    mapM_ addDeprecated deprs)
           *> ZipSink (
                let loop i =
                        await >>= maybe (return ()) (const $ go $ i + 1)
                    go i = do
                        when (i `mod` 500 == 0)
                            $ putStrLn $ concat
                                [ "Processed "
                                , tshow i
                                , " packages"
                                ]
                        loop i
                 in loop (0 :: Int))
            )
        sourceBuildPlans root $$ mapM_C (\(sname, fp', eval) -> flip runSqlPool pool $ do
            let (typ, action) =
                    case eval of
                        Left bp -> ("build-plan", liftIO bp >>= addPlan sname fp')
                        Right dm -> ("doc-map", liftIO dm >>= addDocMap sname)
            let i = Imported sname typ
            eres <- insertBy i
            case eres of
                Left _ -> putStrLn $ "Skipping: " ++ tshow fp'
                Right _ -> action
            )
        flip runSqlPool pool $ mapM_ (flip rawExecute []) ["COMMIT", "VACUUM", "BEGIN"]

getDeprecated' :: [Deprecation] -> Tar.Entry -> [Deprecation]
getDeprecated' orig e =
    case (Tar.entryPath e, Tar.entryContent e) of
        ("deprecated.yaml", Tar.NormalFile lbs _) ->
            case decode $ toStrict lbs of
                Just x -> x
                Nothing -> orig
        _ -> orig

addDeprecated :: Deprecation -> SqlPersistT (ResourceT IO) ()
addDeprecated (Deprecation name others) = do
    name' <- getPackageId name
    others' <- mapM getPackageId $ setToList others
    insert_ $ Deprecated name' others'

getPackageId :: MonadIO m => Text -> ReaderT SqlBackend m (Key Package)
getPackageId x = do
    keys' <- selectKeysList [PackageName ==. x] [LimitTo 1]
    case keys' of
        k:_ -> return k
        [] -> insert Package
            { packageName = x
            , packageLatest = "unknown"
            , packageSynopsis = "Metadata not found"
            , packageDescription = "Metadata not found"
            , packageChangelog = mempty
            , packageAuthor = ""
            , packageMaintainer = ""
            , packageHomepage = ""
            , packageLicenseName = ""
            }

addPackage :: Tar.Entry -> SqlPersistT (ResourceT IO) ()
addPackage e =
    case ("packages/" `isPrefixOf` fp && takeExtension fp == ".yaml", Tar.entryContent e) of
        (True, Tar.NormalFile lbs _) | Just pi <- decode $ toStrict lbs -> do
            let p = Package
                    { packageName = pack base
                    , packageLatest = display $ piLatest pi
                    , packageSynopsis = piSynopsis pi
                    , packageDescription = renderContent (piDescription pi) (piDescriptionType pi)
                    , packageChangelog = renderContent (piChangeLog pi) (piChangeLogType pi)
                    , packageAuthor = piAuthor pi
                    , packageMaintainer = piMaintainer pi
                    , packageHomepage = piHomepage pi
                    , packageLicenseName = piLicenseName pi
                    }

            mp <- getBy $ UniquePackage $ packageName p
            pid <- case mp of
                Just (Entity pid _) -> do
                    replace pid p
                    return pid
                Nothing -> insert p
            deleteWhere [DepUser ==. pid]
            forM_ (mapToList $ piBasicDeps pi) $ \(uses, range) -> insert_ Dep
                { depUser = pid
                , depUses = display uses
                , depRange = display range
                }
        _ -> return ()
  where
    fp = Tar.entryPath e
    base = takeBaseName fp

    renderContent txt "markdown" = markdown
                                    (def { msAddHeadingId = True })
                                    (fromStrict txt)
    renderContent txt "haddock" = renderHaddock txt
    renderContent txt _ = toHtml $ Textarea txt

addPlan :: SnapName -> FilePath -> BuildPlan -> SqlPersistT (ResourceT IO) ()
addPlan name fp bp = do
    putStrLn $ "Adding build plan: " ++ toPathPiece name
    created <-
        case name of
            SNNightly d -> return d
            SNLts _ _ -> do
                let cp' = proc "git"
                        [ "log"
                        , "--format=%ad"
                        , "--date=short"
                        , encodeString $ filename fp
                        ]
                    cp = cp' { cwd = Just $ encodeString $ directory fp }
                t <- withCheckedProcess cp $ \ClosedStream out ClosedStream ->
                    out $$ decodeUtf8C =$ foldC
                case readMay $ concat $ take 1 $ words t of
                    Just created -> return created
                    Nothing -> do
                        putStrLn $ "Warning: unknown git log output: " ++ tshow t
                        return $ fromGregorian 1970 1 1
    sid <- insert Snapshot
        { snapshotName = name
        , snapshotGhc = display $ siGhcVersion $ bpSystemInfo bp
        , snapshotCreated = created
        }
    forM_ allPackages $ \(display -> pname, (display -> version, isCore)) -> do
        pid <- getPackageId pname
        insert_ SnapshotPackage
            { snapshotPackageSnapshot = sid
            , snapshotPackagePackage = pid
            , snapshotPackageIsCore = isCore
            , snapshotPackageVersion = version
            }
    case name of
        SNLts x y -> insert_ Lts
            { ltsSnap = sid
            , ltsMajor = x
            , ltsMinor = y
            }
        SNNightly d -> insert_ Nightly
            { nightlySnap = sid
            , nightlyDay = d
            }
  where
    allPackages = mapToList
        $ fmap (, True) (siCorePackages $ bpSystemInfo bp)
       ++ fmap ((, False) . ppVersion) (bpPackages bp)

addDocMap :: SnapName -> DocMap -> SqlPersistT (ResourceT IO) ()
addDocMap name dm = do
    [sid] <- selectKeysList [SnapshotName ==. name] []
    putStrLn $ "Adding doc map: " ++ toPathPiece name
    forM_ (mapToList dm) $ \(pkg, pd) -> do
        [pid] <- selectKeysList [PackageName ==. pkg] []
        [spid] <- selectKeysList [SnapshotPackageSnapshot ==. sid, SnapshotPackagePackage ==. pid] []
        forM_ (mapToList $ pdModules pd) $ \(mname, _paths) ->
            insert_ Module
                { modulePackage = spid
                , moduleName = mname
                }

run :: GetStackageDatabase m => SqlPersistT IO a -> m a
run inner = do
    StackageDatabase pool <- getStackageDatabase
    liftIO $ runSqlPool inner pool

newestSnapshot :: GetStackageDatabase m => SnapshotBranch -> m (Maybe SnapName)
newestSnapshot LtsBranch = map (uncurry SNLts) <$> newestLTS
newestSnapshot NightlyBranch = map SNNightly <$> newestNightly
newestSnapshot (LtsMajorBranch x) = map (SNLts x) <$> newestLTSMajor x

newestLTS :: GetStackageDatabase m => m (Maybe (Int, Int))
newestLTS =
    run $ liftM (fmap go) $ selectFirst [] [Desc LtsMajor, Desc LtsMinor]
  where
    go (Entity _ lts) = (ltsMajor lts, ltsMinor lts)

newestLTSMajor :: GetStackageDatabase m => Int -> m (Maybe Int)
newestLTSMajor x =
    run $ liftM (fmap $ ltsMinor . entityVal) $ selectFirst [LtsMajor ==. x] [Desc LtsMinor]

ltsMajorVersions :: GetStackageDatabase m => m [(Int, Int)]
ltsMajorVersions =
    run $ liftM (dropOldMinors . map (toPair . entityVal))
        $ selectList [] [Desc LtsMajor, Desc LtsMinor]
  where
    toPair (Lts _ x y) = (x, y)

    dropOldMinors [] = []
    dropOldMinors (l@(x, _):rest) =
        l : dropOldMinors (dropWhile sameMinor rest)
      where
        sameMinor (y, _) = x == y

newestNightly :: GetStackageDatabase m => m (Maybe Day)
newestNightly =
    run $ liftM (fmap $ nightlyDay . entityVal) $ selectFirst [] [Desc NightlyDay]

-- | Get the snapshot which precedes the given one with respect to it's branch (nightly/lts)
snapshotBefore :: GetStackageDatabase m => SnapName -> m (Maybe (SnapshotId, SnapName))
snapshotBefore (SNLts x y)     = ltsBefore x y
snapshotBefore (SNNightly day) = nightlyBefore day

nightlyBefore :: GetStackageDatabase m => Day -> m (Maybe (SnapshotId, SnapName))
nightlyBefore day = do
    run $ liftM (fmap go) $ selectFirst [NightlyDay <. day] [Desc NightlyDay]
  where
    go (Entity _ nightly) = (nightlySnap nightly, SNNightly $ nightlyDay nightly)

ltsBefore :: GetStackageDatabase m => Int -> Int -> m (Maybe (SnapshotId, SnapName))
ltsBefore x y = do
    run $ liftM (fmap go) $ selectFirst
        ( [LtsMajor <=. x, LtsMinor <. y] ||.
          [LtsMajor <. x]
        )
        [Desc LtsMajor, Desc LtsMinor]
  where
    go (Entity _ lts) = (ltsSnap lts, SNLts (ltsMajor lts) (ltsMinor lts))

lookupSnapshot :: GetStackageDatabase m => SnapName -> m (Maybe (Entity Snapshot))
lookupSnapshot name = run $ getBy $ UniqueSnapshot name

snapshotTitle :: Snapshot -> Text
snapshotTitle s = prettyName (snapshotName s) (snapshotGhc s)

prettyName :: SnapName -> Text -> Text
prettyName name ghc = concat [prettyNameShort name, " (ghc-", ghc, ")"]

prettyNameShort :: SnapName -> Text
prettyNameShort name =
    case name of
        SNLts x y -> concat ["LTS Haskell ", tshow x, ".", tshow y]
        SNNightly d -> "Stackage Nightly " ++ tshow d

getAllPackages :: GetStackageDatabase m => m [(Text, Text, Text)] -- FIXME add information on whether included in LTS and Nightly
getAllPackages = liftM (map toPair) $ run $ do
    E.select $ E.from $ \p -> do
        E.orderBy [E.asc $ E.lower_ $ p E.^. PackageName]
        return
            ( p E.^. PackageName
            , p E.^. PackageLatest
            , p E.^. PackageSynopsis
            )
  where
    toPair (E.Value x, E.Value y, E.Value z) = (x, y, z)

data PackageListingInfo = PackageListingInfo
    { pliName :: !Text
    , pliVersion :: !Text
    , pliSynopsis :: !Text
    , pliIsCore :: !Bool
    }

instance A.ToJSON PackageListingInfo where
   toJSON PackageListingInfo{..} =
       A.object [ "name"     A..= pliName
                , "version"  A..= pliVersion
                , "synopsis" A..= pliSynopsis
                , "isCore"   A..= pliIsCore
                ]

getPackages :: GetStackageDatabase m => SnapshotId -> m [PackageListingInfo]
getPackages sid = liftM (map toPLI) $ run $ do
    E.select $ E.from $ \(p,sp) -> do
        E.where_ $
            (p E.^. PackageId E.==. sp E.^. SnapshotPackagePackage) E.&&.
            (sp E.^. SnapshotPackageSnapshot E.==. E.val sid)
        E.orderBy [E.asc $ E.lower_ $ p E.^. PackageName]
        return
            ( p E.^. PackageName
            , p E.^. PackageSynopsis
            , sp E.^. SnapshotPackageVersion
            , sp E.^. SnapshotPackageIsCore
            )
  where
    toPLI (E.Value name, E.Value synopsis, E.Value version, E.Value isCore) = PackageListingInfo
        { pliName = name
        , pliVersion = version
        , pliSynopsis = synopsis
        , pliIsCore = isCore
        }

getPackageVersionBySnapshot
  :: GetStackageDatabase m
  => SnapshotId -> Text -> m (Maybe Text)
getPackageVersionBySnapshot sid name = liftM (listToMaybe . map toPLI) $ run $ do
    E.select $ E.from $ \(p,sp) -> do
        E.where_ $
            (p E.^. PackageId E.==. sp E.^. SnapshotPackagePackage) E.&&.
            (sp E.^. SnapshotPackageSnapshot E.==. E.val sid) E.&&.
            (E.lower_ (p E.^. PackageName) E.==. E.lower_ (E.val name))
        E.orderBy [E.asc $ E.lower_ $ p E.^. PackageName]
        return
            ( sp E.^. SnapshotPackageVersion
            )
  where
    toPLI (E.Value version) = version

data ModuleListingInfo = ModuleListingInfo
    { mliName :: !Text
    , mliPackageVersion :: !Text
    }

getSnapshotModules
    :: GetStackageDatabase m
    => SnapshotId
    -> m [ModuleListingInfo]
getSnapshotModules sid = liftM (map toMLI) $ run $ do
    E.select $ E.from $ \(p,sp,m) -> do
        E.where_ $
            (p E.^. PackageId E.==. sp E.^. SnapshotPackagePackage) E.&&.
            (sp E.^. SnapshotPackageSnapshot E.==. E.val sid) E.&&.
            (m E.^. ModulePackage E.==. sp E.^. SnapshotPackageId)
        E.orderBy
            [ E.asc $ m E.^. ModuleName
            , E.asc $ E.lower_ $ p E.^. PackageName
            ]
        return
            ( m E.^. ModuleName
            , p E.^. PackageName
            , sp E.^. SnapshotPackageVersion
            )
  where
    toMLI (E.Value name, E.Value pkg, E.Value version) = ModuleListingInfo
        { mliName = name
        , mliPackageVersion = concat [pkg, "-", version]
        }

getPackageModules
    :: GetStackageDatabase m
    => SnapName
    -> Text
    -> m [Text]
getPackageModules sname pname = run $ do
    sids <- selectKeysList [SnapshotName ==. sname] []
    pids <- selectKeysList [PackageName ==. pname] []
    case (,) <$> listToMaybe sids <*> listToMaybe pids of
        Nothing -> return []
        Just (sid, pid) -> do
            spids <- selectKeysList
                [ SnapshotPackageSnapshot ==. sid
                , SnapshotPackagePackage ==. pid
                ] []
            case spids of
                spid:_ -> map (moduleName . entityVal)
                      <$> selectList [ModulePackage ==. spid] [Asc ModuleName]
                [] -> return []

lookupSnapshotPackage
    :: GetStackageDatabase m
    => SnapshotId
    -> Text
    -> m (Maybe (Entity SnapshotPackage))
lookupSnapshotPackage sid pname = run $ do
    mp <- getBy $ UniquePackage pname
    case mp of
        Nothing -> return Nothing
        Just (Entity pid _) -> getBy $ UniqueSnapshotPackage sid pid

getDeprecated :: GetStackageDatabase m => Text -> m (Bool, [Text])
getDeprecated name = run $ do
    pids <- selectKeysList [PackageName ==. name] []
    case pids of
        [pid] -> do
            mdep <- getBy $ UniqueDeprecated pid
            case mdep of
                Nothing -> return defRes
                Just (Entity _ (Deprecated _ favors)) -> do
                    names <- mapM getName favors
                    return (True, catMaybes names)
        _ -> return defRes
  where
    defRes = (False, [])

    getName = fmap (fmap packageName) . get

data LatestInfo = LatestInfo
    { liSnapName :: !SnapName
    , liVersion :: !Text
    , liGhc :: !Text
    }
    deriving Show

getLatests :: GetStackageDatabase m
           => Text -- ^ package name
           -> m [LatestInfo]
getLatests pname = run $ do
    mlts <- latestHelper pname
        (\s ln -> s E.^. SnapshotId E.==. ln E.^. LtsSnap)
        (\_ ln ->
            [ E.desc $ ln E.^. LtsMajor
            , E.desc $ ln E.^. LtsMinor
            ])
    mnightly <- latestHelper pname
        (\s ln -> s E.^. SnapshotId E.==. ln E.^. NightlySnap)
        (\s _ln -> [E.desc $ s E.^. SnapshotCreated])
    return $ concat [mlts, mnightly]

latestHelper
    :: (From E.SqlQuery E.SqlExpr SqlBackend t, MonadIO m, Functor m)
    => Text
    -> (E.SqlExpr (Entity Snapshot) -> t -> E.SqlExpr (E.Value Bool))
    -> (E.SqlExpr (Entity Snapshot) -> t -> [E.SqlExpr E.OrderBy])
    -> ReaderT SqlBackend m [LatestInfo]
latestHelper pname clause order = fmap (fmap toLatest) $ E.select $ E.from $ \(s,ln,p,sp) -> do
    E.where_ $
        clause s ln E.&&.
        (s E.^. SnapshotId E.==. sp E.^. SnapshotPackageSnapshot) E.&&.
        (p E.^. PackageName E.==. E.val pname) E.&&.
        (p E.^. PackageId E.==. sp E.^. SnapshotPackagePackage)
    E.orderBy $ order s ln
    E.limit 1
    return
        ( s E.^. SnapshotName
        , s E.^. SnapshotGhc
        , sp E.^. SnapshotPackageVersion
        )
  where
    toLatest (E.Value sname, E.Value ghc, E.Value version) = LatestInfo
        { liSnapName = sname
        , liVersion = version
        , liGhc = ghc
        }

getDeps :: GetStackageDatabase m => Text -> m [(Text, Text)]
getDeps pname = run $ do
    mp <- getBy $ UniquePackage pname
    case mp of
        Nothing -> return []
        Just (Entity pid _) -> fmap (map toPair) $ E.select $ E.from $ \d -> do
            E.where_ $
                (d E.^. DepUser E.==. E.val pid)
            E.orderBy [E.asc $ d E.^. DepUses]
            return (d E.^. DepUses, d E.^. DepRange)
  where
    toPair (E.Value x, E.Value y) = (x, y)

getRevDeps :: GetStackageDatabase m => Text -> m [(Text, Text)]
getRevDeps pname = run $ do
    fmap (map toPair) $ E.select $ E.from $ \(d,p) -> do
        E.where_ $
            (d E.^. DepUses E.==. E.val pname) E.&&.
            (d E.^. DepUser E.==. p E.^. PackageId)
        E.orderBy [E.asc $ p E.^. PackageName]
        return (p E.^. PackageName, d E.^. DepRange)
  where
    toPair (E.Value x, E.Value y) = (x, y)

getPackage :: GetStackageDatabase m => Text -> m (Maybe (Entity Package))
getPackage = run . getBy . UniquePackage

getSnapshotsForPackage
    :: GetStackageDatabase m
    => Text
    -> m [(Snapshot, Text)] -- version
getSnapshotsForPackage pname = run $ do
    pid <- getPackageId pname
    sps <- selectList [SnapshotPackagePackage ==. pid] []
    fmap catMaybes $ forM sps $ \(Entity _ sp) -> do
        let sid = snapshotPackageSnapshot sp
        ms <- get sid
        return $ case ms of
            Nothing -> Nothing
            Just s -> Just (s, snapshotPackageVersion sp)

-- | Count snapshots that belong to a specific SnapshotBranch
countSnapshots :: (GetStackageDatabase m) => Maybe SnapshotBranch -> m Int
countSnapshots Nothing                   = run $ count ([] :: [Filter Snapshot])
countSnapshots (Just NightlyBranch)      = run $ count ([] :: [Filter Nightly])
countSnapshots (Just LtsBranch)          = run $ count ([] :: [Filter Lts])
countSnapshots (Just (LtsMajorBranch x)) = run $ count [LtsMajor ==. x]

-- | Get snapshots that belong to a specific SnapshotBranch
getSnapshots :: (GetStackageDatabase m)
             => Maybe SnapshotBranch
             -> Int -- ^ limit
             -> Int -- ^ offset
             -> m [Entity Snapshot]
getSnapshots mBranch l o = run $ case mBranch of
    Nothing -> selectList [] [LimitTo l, OffsetBy o, Desc SnapshotCreated]
    Just NightlyBranch ->
        E.select $ E.from $ \(nightly `E.InnerJoin` snapshot) -> do
            E.on $ nightly E.^. NightlySnap E.==. snapshot E.^. SnapshotId
            E.orderBy [E.desc (nightly E.^. NightlyDay)]
            E.limit $ fromIntegral l
            E.offset $ fromIntegral o
            pure snapshot
    Just LtsBranch -> do
        E.select $ E.from $ \(lts `E.InnerJoin` snapshot) -> do
            E.on $ lts E.^. LtsSnap E.==. snapshot E.^. SnapshotId
            E.orderBy [ E.desc (lts E.^. LtsMajor)
                      , E.desc (lts E.^. LtsMinor) ]
            E.limit $ fromIntegral l
            E.offset $ fromIntegral o
            pure snapshot
    Just (LtsMajorBranch v) -> do
        E.select $ E.from $ \(lts `E.InnerJoin` snapshot) -> do
            E.on $ lts E.^. LtsSnap E.==. snapshot E.^. SnapshotId
            E.orderBy [E.desc (lts E.^. LtsMinor)]
            E.where_ ((lts E.^. LtsMajor) E.==. (E.val v))
            E.limit $ fromIntegral l
            E.offset $ fromIntegral o
            pure snapshot

last5Lts5Nightly :: GetStackageDatabase m => m [SnapName]
last5Lts5Nightly = run $ do
    ls <- selectList [] [Desc LtsMajor, Desc LtsMinor, LimitTo 5]
    ns <- selectList [] [Desc NightlyDay, LimitTo 5]
    return $ map l ls ++ map n ns
  where
    l (Entity _ x) = SNLts (ltsMajor x) (ltsMinor x)
    n (Entity _ x) = SNNightly (nightlyDay x)

snapshotsJSON :: GetStackageDatabase m => m A.Value
snapshotsJSON = do
    mlatestNightly <- newestNightly
    ltses <- ltsMajorVersions
    let lts = case ltses of
            [] -> []
            majorVersions@(latest:_) ->
                   ("lts" A..= printLts latest)
                 : map toObj majorVersions
        nightly = case mlatestNightly of
            Nothing -> id
            Just n -> (("nightly" A..= printNightly n):)
    return $ A.object $ nightly lts
  where
    toObj lts@(major, _) =
        pack ("lts-" ++ show major) A..= printLts lts
    printLts (major, minor) =
        "lts-" ++ show major ++ "." ++ show minor

    printNightly day = "nightly-" ++ tshow day

getPackageCount :: GetStackageDatabase m
                => SnapshotId
                -> m Int
getPackageCount sid = run $ count [SnapshotPackageSnapshot ==. sid]
