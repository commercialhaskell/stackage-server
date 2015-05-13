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
    ) where

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
    description Html
    changelog Html
    UniquePackage name
SnapshotPackage
    snapshot SnapshotId
    package PackageId
    isCore Bool
    version Text
    UniqueSnapshotPackage snapshot package
Dep
    user PackageId
    usedBy PackageId
    range Text
    UniqueDep user usedBy
Deprecated
    package PackageId
    inFavorOf [PackageId]
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

sourceBuildPlans :: MonadResource m => FilePath -> Producer m (SnapName, BuildPlan)
sourceBuildPlans root = do
    forM_ ["lts-haskell", "stackage-nightly"] $ \dir -> do
        dir <- liftIO $ cloneOrUpdate root "fpco" dir
        sourceDirectory dir =$= concatMapMC go
  where
    go fp | Just name <- nameFromFP fp = liftIO $ do
        bp <- decodeFileEither (fpToString fp) >>= either throwM return
        return $ Just (name, bp)
    go _ = return Nothing

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
    runSqlPool (runMigration migrateAll) pool
    root <- liftIO $ fmap (</> "database") $ fmap fpFromString $ getAppUserDataDirectory "stackage"
    F.createTree root
    runResourceT $ do
        flip runSqlPool pool $ sourcePackages root $$ getZipSink
            ( ZipSink (mapM_C addPackage)
           *> ZipSink (foldlC getDeprecated [] >>= lift . mapM_ addDeprecated)
            )
        sourceBuildPlans root $$ mapM_C (flip runSqlPool pool . addPlan)

getDeprecated :: [Deprecation] -> Tar.Entry -> [Deprecation]
getDeprecated orig e =
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
            }

addPackage :: Tar.Entry -> SqlPersistT (ResourceT IO) ()
addPackage e =
    case ("packages/" `isPrefixOf` fp && takeExtension fp == ".yaml", Tar.entryContent e) of
        (True, Tar.NormalFile lbs _) | Just pi <- decode $ toStrict lbs ->
            insert_ Package
                { packageName = pack base
                , packageLatest = display $ piLatest pi
                , packageSynopsis = piSynopsis pi
                , packageDescription = renderContent (piDescription pi) (piDescriptionType pi)
                , packageChangelog = renderContent (piChangeLog pi) (piChangeLogType pi)
                }
        _ -> return ()
  where
    fp = Tar.entryPath e
    base = takeBaseName fp

    renderContent txt "markdown" = toHtml $ Markdown $ fromStrict txt
    renderContent txt "haddock" = renderHaddock txt
    renderContent txt _ = toHtml $ Textarea txt

addPlan :: (SnapName, BuildPlan) -> SqlPersistT (ResourceT IO) ()
addPlan (name, bp) = do
    sid <- insert Snapshot
        { snapshotName = name
        , snapshotGhc = display $ siGhcVersion $ bpSystemInfo bp
        , snapshotCreated =
            case name of
                SNNightly d -> d
                SNLts _ _ -> fromGregorian 1970 1 1 -- FIXME
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
snapshotTitle s =
    concat [base, " - GHC ", snapshotGhc s]
  where
    base =
        case snapshotName s of
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
