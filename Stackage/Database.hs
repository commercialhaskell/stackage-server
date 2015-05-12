module Stackage.Database
    ( StackageDatabase
    , SnapName (..)
    , Snapshot (..)
    , loadStackageDatabase
    , newestLTS
    , newestLTSMajor
    , newestNightly
    , lookupSnapshot
    , snapshotTitle
    , PackageListingInfo (..)
    , getPackages
    ) where

import ClassyPrelude.Conduit
import Data.Time
import Stackage.Database.Types
import System.Directory (getAppUserDataDirectory, getTemporaryDirectory)
import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as F
import Data.Conduit.Process
import Stackage.Types
import Web.PathPieces (fromPathPiece)
import Data.Yaml (decodeFileEither)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.Logger
import Control.Concurrent (forkIO)
import System.IO.Temp
import qualified Database.Esqueleto as E

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
    UniquePackage name
SnapshotPackage
    snapshot SnapshotId
    package PackageId
    isCore Bool
    version Text
    UniqueSnapshotPackage snapshot package
|]

newtype StackageDatabase = StackageDatabase ConnectionPool

sourceBuildPlans :: MonadResource m => Producer m (SnapName, BuildPlan)
sourceBuildPlans = do
    root <- liftIO $ fmap (</> "database") $ fmap fpFromString $ getAppUserDataDirectory "stackage"
    liftIO $ F.createTree root
    forM_ ["lts-haskell", "stackage-nightly"] $ \dir -> do
        dir <- liftIO $ cloneOrUpdate root dir
        sourceDirectory dir =$= concatMapMC go
  where
    go fp | Just name <- nameFromFP fp = liftIO $ do
        bp <- decodeFileEither (fpToString fp) >>= either throwM return
        return $ Just (name, bp)
    go _ = return Nothing

    nameFromFP fp = do
        base <- stripSuffix ".yaml" $ fpToText $ filename fp
        fromPathPiece base

    cloneOrUpdate root name = do
        exists <- F.isDirectory dest
        if exists
            then do
                let run = runIn dest
                run "git" ["fetch"]
                run "git" ["reset", "--hard", "origin/master"]
            else runIn root "git" ["clone", url, name]
        return dest
      where
        url = "https://github.com/fpco/" ++ name ++ ".git"
        dest = root </> fpFromString name

        runIn dir cmd args =
            withCheckedProcess cp $ \ClosedStream Inherited Inherited -> return ()
          where
            cp = (proc cmd args) { cwd = Just $ fpToString dir }

loadStackageDatabase :: MonadIO m
                     => Bool -- ^ block until all snapshots added?
                     -> m StackageDatabase
loadStackageDatabase toBlock = liftIO $ do
    tmp <- getTemporaryDirectory
    (fp, h) <- openBinaryTempFile "/tmp" "stackage-database.sqlite3"
    hClose h
    pool <- runNoLoggingT $ createSqlitePool (pack fp) 7
    runSqlPool (runMigration migrateAll) pool
    forker $ runResourceT $ sourceBuildPlans $$ mapM_C (flip runSqlPool pool . addPlan)
    return $ StackageDatabase pool
  where
    forker
        | toBlock = id
        | otherwise = void . forkIO

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
        pid <- case mp of
            Nothing -> insert $ Package name "FIXME latest version" "FIXME synopsis"
            Just (Entity pid _) -> return pid
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

run :: MonadIO m => StackageDatabase -> SqlPersistT IO a -> m a
run (StackageDatabase pool) inner = liftIO $ runSqlPool inner pool

newestLTS :: MonadIO m => StackageDatabase -> m (Maybe (Int, Int))
newestLTS db =
    run db $ liftM (fmap go) $ selectFirst [] [Desc LtsMajor, Desc LtsMinor]
  where
    go (Entity _ lts) = (ltsMajor lts, ltsMinor lts)

newestLTSMajor :: MonadIO m => StackageDatabase -> Int -> m (Maybe Int)
newestLTSMajor db x =
    run db $ liftM (fmap $ ltsMinor . entityVal) $ selectFirst [LtsMajor ==. x] [Desc LtsMinor]

newestNightly :: MonadIO m => StackageDatabase -> m (Maybe Day)
newestNightly db =
    run db $ liftM (fmap $ nightlyDay . entityVal) $ selectFirst [] [Desc NightlyDay]

lookupSnapshot :: MonadIO m => StackageDatabase -> SnapName -> m (Maybe (Entity Snapshot))
lookupSnapshot db name = run db $ getBy $ UniqueSnapshot name

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
    }

getPackages :: MonadIO m => StackageDatabase -> SnapshotId -> m [PackageListingInfo]
getPackages db sid = liftM (map toPLI) $ run db $ do
    E.select $ E.from $ \(p,sp) -> do
        E.where_ $
            (p E.^. PackageId E.==. sp E.^. SnapshotPackagePackage) E.&&.
            (sp E.^. SnapshotPackageSnapshot E.==. E.val sid)
        E.orderBy [E.asc $ p E.^. PackageName]
        return
            ( p E.^. PackageName
            , p E.^. PackageSynopsis
            , sp E.^. SnapshotPackageVersion
            )
  where
    toPLI (E.Value name, E.Value synopsis, E.Value version) = PackageListingInfo
        { pliName = name
        , pliVersion = version
        , pliSynopsis = synopsis
        }
