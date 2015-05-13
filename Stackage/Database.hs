module Stackage.Database
    ( StackageDatabase
    , GetStackageDatabase (..)
    , SnapName (..)
    , Snapshot (..)
    , newestLTS
    , newestLTSMajor
    , newestNightly
    , lookupSnapshot
    , snapshotTitle
    , PackageListingInfo (..)
    , getPackages
    , createStackageDatabase
    , openStackageDatabase
    , ModuleListingInfo (..)
    , getSnapshotModules
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
    ) where

import Web.PathPieces (toPathPiece)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Text.Markdown (Markdown (..))
import System.Directory (removeFile)
import Stackage.Database.Haddock
import System.FilePath (takeBaseName, takeExtension)
import ClassyPrelude.Conduit
import Data.Time
import Text.Blaze.Html (Html, toHtml)
import Yesod.Form.Fields (Textarea (..))
import Stackage.Database.Types
import System.Directory (getAppUserDataDirectory, getTemporaryDirectory)
import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as F
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
import Control.Concurrent (forkIO)
import System.IO.Temp
import qualified Database.Esqueleto as E
import Data.Yaml (decode)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
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

newtype StackageDatabase = StackageDatabase ConnectionPool

class MonadIO m => GetStackageDatabase m where
    getStackageDatabase :: m StackageDatabase

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

sourceBuildPlans :: MonadResource m => FilePath -> Producer m (SnapName, FilePath, Either BuildPlan DocMap)
sourceBuildPlans root = do
    forM_ ["lts-haskell", "stackage-nightly"] $ \dir -> do
        dir <- liftIO $ cloneOrUpdate root "fpco" dir
        sourceDirectory dir =$= concatMapMC (go Left)
        let docdir = dir </> "docs"
        whenM (liftIO $ F.isDirectory docdir) $
            sourceDirectory docdir =$= concatMapMC (go Right)
  where
    go wrapper fp | Just name <- nameFromFP fp = liftIO $ do
        bp <- decodeFileEither (fpToString fp) >>= either throwM return
        return $ Just (name, fp, wrapper bp)
    go _ _ = return Nothing

    nameFromFP fp = do
        base <- stripSuffix ".yaml" $ fpToText $ filename fp
        fromPathPiece base

cloneOrUpdate :: FilePath -> String -> String -> IO FilePath
cloneOrUpdate root org name = do
    exists <- F.isDirectory dest
    if exists
        then do
            let run = runIn dest
            run "git" ["fetch"]
            run "git" ["reset", "--hard", "origin/master"]
        else runIn root "git" ["clone", url, name]
    return dest
  where
    url = "https://github.com/" ++ org ++ "/" ++ name ++ ".git"
    dest = root </> fpFromString name

runIn :: FilePath -> String -> [String] -> IO ()
runIn dir cmd args =
    withCheckedProcess cp $ \ClosedStream Inherited Inherited -> return ()
  where
    cp = (proc cmd args) { cwd = Just $ fpToString dir }

openStackageDatabase :: MonadIO m => FilePath -> m StackageDatabase
openStackageDatabase fp = liftIO $ fmap StackageDatabase $ runNoLoggingT $ createSqlitePool (fpToText fp) 7

createStackageDatabase :: MonadIO m => FilePath -> m ()
createStackageDatabase fp = liftIO $ do
    void $ tryIO $ removeFile $ fpToString fp
    StackageDatabase pool <- openStackageDatabase fp
    putStrLn "Initial migration"
    runSqlPool (runMigration migrateAll) pool
    root <- liftIO $ fmap (</> "database") $ fmap fpFromString $ getAppUserDataDirectory "stackage"
    F.createTree root
    runResourceT $ do
        flip runSqlPool pool $ sourcePackages root $$ getZipSink
            ( ZipSink (mapM_C addPackage)
           *> ZipSink (foldlC getDeprecated' [] >>= lift . mapM_ addDeprecated)
            )
        sourceBuildPlans root $$ mapM_C (flip runSqlPool pool . addPlan)

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

getPackageId x = do
    keys <- selectKeysList [PackageName ==. x] [LimitTo 1]
    case keys of
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
            pid <- insert Package
                { packageName = pack base
                , packageLatest = display $ piLatest pi
                , packageSynopsis = piSynopsis pi
                , packageDescription = renderContent (piDescription pi) (piDescriptionType pi)
                , packageChangelog = renderContent (piChangeLog pi) (piChangeLogType pi)
                , packageAuthor = "FIXME author"
                , packageMaintainer = "FIXME maintainer"
                , packageHomepage = "FIXME homepage"
                , packageLicenseName = "FIXME license name"
                }
            forM_ (mapToList $ piBasicDeps pi) $ \(uses, range) -> insert_ Dep
                { depUser = pid
                , depUses = display uses
                , depRange = display range
                }
        _ -> return ()
  where
    fp = Tar.entryPath e
    base = takeBaseName fp

    renderContent txt "markdown" = toHtml $ Markdown $ fromStrict txt
    renderContent txt "haddock" = renderHaddock txt
    renderContent txt _ = toHtml $ Textarea txt

addPlan :: (SnapName, FilePath, Either BuildPlan DocMap) -> SqlPersistT (ResourceT IO) ()
addPlan (name, fp, Left bp) = do
    putStrLn $ "Adding build plan: " ++ toPathPiece name
    created <-
        case name of
            SNNightly d -> return d
            SNLts _ _ -> do
                let cp' = proc "git"
                        [ "log"
                        , "--format=%ad"
                        , "--date=short"
                        , fpToString $ filename fp
                        ]
                    cp = cp' { cwd = Just $ fpToString $ directory fp }
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
    forM_ allPackages $ \(display -> name, (display -> version, isCore)) -> do
        mp <- getBy $ UniquePackage name
        pid <- getPackageId name
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
addPlan (name, _, Right dm) = do
    [sid] <- selectKeysList [SnapshotName ==. name] []
    putStrLn $ "Adding doc map: " ++ toPathPiece name
    forM_ (mapToList dm) $ \(pkg, pd) -> do
        [pid] <- selectKeysList [PackageName ==. pkg] []
        [spid] <- selectKeysList [SnapshotPackageSnapshot ==. sid, SnapshotPackagePackage ==. pid] []
        forM_ (mapToList $ pdModules pd) $ \(name, paths) ->
            insert_ Module
                { modulePackage = spid
                , moduleName = name
                }

run :: GetStackageDatabase m => SqlPersistT IO a -> m a
run inner = do
    StackageDatabase pool <- getStackageDatabase
    liftIO $ runSqlPool inner pool

newestLTS :: GetStackageDatabase m => m (Maybe (Int, Int))
newestLTS =
    run $ liftM (fmap go) $ selectFirst [] [Desc LtsMajor, Desc LtsMinor]
  where
    go (Entity _ lts) = (ltsMajor lts, ltsMinor lts)

newestLTSMajor :: GetStackageDatabase m => Int -> m (Maybe Int)
newestLTSMajor x =
    run $ liftM (fmap $ ltsMinor . entityVal) $ selectFirst [LtsMajor ==. x] [Desc LtsMinor]

newestNightly :: GetStackageDatabase m => m (Maybe Day)
newestNightly =
    run $ liftM (fmap $ nightlyDay . entityVal) $ selectFirst [] [Desc NightlyDay]

lookupSnapshot :: GetStackageDatabase m => SnapName -> m (Maybe (Entity Snapshot))
lookupSnapshot name = run $ getBy $ UniqueSnapshot name

snapshotTitle :: Snapshot -> Text
snapshotTitle s = prettyName (snapshotName s) (snapshotGhc s)

prettyName :: SnapName -> Text -> Text
prettyName name ghc =
    concat [base, " - GHC ", ghc]
  where
    base =
        case name of
            SNLts x y -> concat ["LTS Haskell ", tshow x, ".", tshow y]
            SNNightly d -> "Stackage Nightly " ++ tshow d

data PackageListingInfo = PackageListingInfo
    { pliName :: !Text
    , pliVersion :: !Text
    , pliSynopsis :: !Text
    , pliIsCore :: !Bool
    }

getPackages :: GetStackageDatabase m => SnapshotId -> m [PackageListingInfo]
getPackages sid = liftM (map toPLI) $ run $ do
    E.select $ E.from $ \(p,sp) -> do
        E.where_ $
            (p E.^. PackageId E.==. sp E.^. SnapshotPackagePackage) E.&&.
            (sp E.^. SnapshotPackageSnapshot E.==. E.val sid)
        E.orderBy [E.asc $ p E.^. PackageName]
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
            , E.asc $ p E.^. PackageName
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
    mnightly <- latestHelper pname $ \s ln -> s E.^. SnapshotId E.==. ln E.^. NightlySnap
    mlts <- latestHelper pname $ \s ln -> s E.^. SnapshotId E.==. ln E.^. LtsSnap
    return $ concat [mnightly, mlts]

latestHelper pname clause = fmap (fmap toLatest) $ E.select $ E.from $ \(s,ln,p,sp) -> do
    E.where_ $
        clause s ln E.&&.
        (s E.^. SnapshotId E.==. sp E.^. SnapshotPackageSnapshot) E.&&.
        (p E.^. PackageName E.==. E.val pname) E.&&.
        (p E.^. PackageId E.==. sp E.^. SnapshotPackagePackage)
    E.orderBy [E.desc $ s E.^. SnapshotCreated]
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
    Just (Entity pid _) <- getBy $ UniquePackage pname
    fmap (map toPair) $ E.select $ E.from $ \d -> do
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
