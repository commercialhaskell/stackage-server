{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Data.Pool (destroyAllResources)
import Database.Persist.Postgresql (PostgresConf(..), createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool)
import Gauge
import Pantry.Internal.Stackage (PackageNameP(..))
import RIO
import Settings (getAppSettings, AppSettings(..), configSettingsYmlValue)
import Stackage.Database.Query
import Stackage.Database.Schema (withStackageDatabase, runDatabase)
import Stackage.Database.Types (LatestInfo, SnapName(..), SnapshotPackageInfo(..))
import Yesod.Default.Config2

main :: IO ()
main = do
    appSettings <- getAppSettings
    let pgConf =
            PostgresConf
                { pgPoolSize = appPostgresPoolsize appSettings
                , pgConnStr = encodeUtf8 $ appPostgresString appSettings
                }
    let snapName = SNLts 16 4
    mSnapInfo <-
        runSimpleApp $
        withStackageDatabase
            True
            pgConf
            (\db -> runDatabase db $ getSnapshotPackageInfoQuery snapName (PackageNameP "yesod"))
    let snapInfo = fromMaybe (error "snapInfo not retrieved") mSnapInfo
    defaultMain [benchs snapInfo]

runBenchApp :: ConnectionPool -> ReaderT SqlBackend (RIO SimpleApp) a -> IO a
runBenchApp pool m = runSimpleApp $ runSqlPool m pool

createBenchPool :: IO ConnectionPool
createBenchPool = do
    baSettings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
    pool <-
        runNoLoggingT $
        createPostgresqlPool
            (encodeUtf8 $ appPostgresString baSettings)
            (appPostgresPoolsize baSettings)
    pure pool

releasePool :: ConnectionPool -> IO ()
releasePool = destroyAllResources

-- TODO: Upstream fix ? Or add new function to gauge (although it
-- seems it might be a breaking change there) ?
instance NFData ConnectionPool where
    rnf _ = ()

getLatestsBench :: Benchmark
getLatestsBench =
    bench "getLatests" $
    perBatchEnvWithCleanup
        (\runs -> createBenchPool)
        (\_ pool -> releasePool pool)
        (\pool -> runBenchApp pool (void $ getLatests $ PackageNameP "yesod"))

getDeprecatedBench :: Benchmark
getDeprecatedBench =
    bench "getDeprecated" $
    perBatchEnvWithCleanup
        (\runs -> createBenchPool)
        (\_ pool -> releasePool pool)
        (\pool -> runBenchApp pool (void $ getDeprecatedQuery $ PackageNameP "yesod"))

getSnapshotPackageLatestVersionBench :: Benchmark
getSnapshotPackageLatestVersionBench =
    bench "getSnapshotPackageLatestVersion" $
    perBatchEnvWithCleanup
        (\runs -> createBenchPool)
        (\_ pool -> releasePool pool)
        (\pool ->
             runBenchApp pool (void $ getSnapshotPackageLatestVersionQuery $ PackageNameP "yesod"))

getSnapshotPackagePageInfoBench :: SnapshotPackageInfo -> Benchmark
getSnapshotPackagePageInfoBench snapshotInfo =
    bench "getSnapshotPackagePageInfo" $
    perBatchEnvWithCleanup
        (\runs -> createBenchPool)
        (\_ pool -> releasePool pool)
        (\pool -> runBenchApp pool (void $ getSnapshotPackagePageInfoQuery snapshotInfo 40))

getPackageInfoBench :: SnapshotPackageInfo -> Benchmark
getPackageInfoBench snapInfo =
    bench "getPackageInfo" $
    perBatchEnvWithCleanup
        (\runs -> createBenchPool)
        (\_ pool -> releasePool pool)
        (\pool -> runBenchApp pool (void $ getPackageInfoQuery (Right snapInfo)))

getHackageLatestVersionBench :: Benchmark
getHackageLatestVersionBench =
    bench "getHackageLatestVersion" $
    perBatchEnvWithCleanup
        (\runs -> createBenchPool)
        (\_ pool -> releasePool pool)
        (\pool -> runBenchApp pool (void $ getHackageLatestVersion $ PackageNameP "yesod"))

benchs :: SnapshotPackageInfo -> Benchmark
benchs snap =
    bgroup
        "SQL Query Benchmark"
        [ getLatestsBench
        , getDeprecatedBench
        , getHackageLatestVersionBench
        , getPackageInfoBench snap
        , getSnapshotPackagePageInfoBench snap
        , getSnapshotPackageLatestVersionBench
        ]
