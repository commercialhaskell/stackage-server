{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Stackage.Database.Query
    (
      -- * Snapshot
      newestSnapshot
    , newestLTS
    , newestLTSMajor
    , newestNightly
    , getSnapshots
    , countSnapshots
    , ltsMajorVersions
    , snapshotBefore
    , lookupSnapshot
    , snapshotTitle
    , snapshotsJSON
    , getLatestLtsByGhc
    , getLatestLtsNameWithHoogle

    , getSnapshotModules
    , getSnapshotPackageModules

    -- * Package

    , getAllPackages
    , getPackagesForSnapshot
    , getPackagesForSnapshotDiff
    , getPackageVersionForSnapshot

    , getLatests
    , getHackageLatestVersion
    , getSnapshotPackageInfo
    , getSnapshotPackageInfoQuery
    , getSnapshotPackageLatestVersion
    , getSnapshotPackageLatestVersionQuery
    , getSnapshotPackagePageInfo
    , getSnapshotPackagePageInfoQuery
    , getPackageInfo
    , getPackageInfoQuery
    , getSnapshotsForPackage
    -- ** Dependencies

    , getForwardDeps
    , getReverseDeps
    , getDepsCount

    -- ** Deprecations

    , getDeprecated
    , getDeprecatedQuery
    , setDeprecations

    -- * Needed for Cron Job
    -- ** Re-exports from Pantry
    , loadBlobById
    , getTreeForKey
    , treeCabal
    , getVersionId
    -- ** Stackage server
    , CabalFileIds
    , addCabalFile
    , getCabalFileIds
    , addSnapshotPackage
    , getHackageCabalByRev0
    , getHackageCabalByKey
    , snapshotMarkUpdated
    , insertSnapshotName
    , markModuleHasDocs
    , insertDeps
    -- ** For Hoogle db creation
    , lastLtsNightlyWithoutHoogleDb
    , getSnapshotPackageCabalBlob
    , checkInsertSnapshotHoogleDb
    ) where

import qualified Data.Aeson as A
import qualified Data.List as L
import Database.Esqueleto
import Database.Esqueleto.Internal.Language (FromPreprocess)
import Database.Esqueleto.Internal.Sql
import Distribution.Types.PackageId (PackageIdentifier(PackageIdentifier))
import Distribution.PackageDescription (packageDescription)
import Distribution.Types.PackageDescription (PackageDescription(package))
import qualified Database.Persist as P
import Pantry.Internal.Stackage (EntityField(..), PackageName,
                                 Version, getBlobKey, getPackageNameById,
                                 getPackageNameId, getTreeForKey, getVersionId,
                                 loadBlobById, storeBlob, mkSafeFilePath, versionVersion)
import RIO hiding (on, (^.))
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as T
import RIO.Time (Day, UTCTime)
import Stackage.Database.PackageInfo
import Stackage.Database.Schema
import Stackage.Database.Types


-- | Construct a pretty title for the snapshot
snapshotTitle :: Snapshot -> Text
snapshotTitle s = snapshotPrettyName (snapshotName s) (snapshotCompiler s)

-- | Get the snapshot from the database.
lookupSnapshot :: GetStackageDatabase env m => SnapName -> m (Maybe (Entity Snapshot))
lookupSnapshot name = run $ getBy $ UniqueSnapshot name

-- | A way to lookup a name of the newest snapshot per type: 'lts', 'lts-x' and 'nightly'. This is
-- used for resolving a snapshot
newestSnapshot :: GetStackageDatabase env m => SnapshotBranch -> m (Maybe SnapName)
newestSnapshot LtsBranch          = fmap (uncurry SNLts) <$> newestLTS
newestSnapshot NightlyBranch      = fmap SNNightly <$> newestNightly
newestSnapshot (LtsMajorBranch x) = fmap (SNLts x) <$> newestLTSMajor x

-- | Get the latest known LTS snapshot
newestLTS :: GetStackageDatabase env m => m (Maybe (Int, Int))
newestLTS =
    run $ liftM (fmap go) $ selectFirst [] [P.Desc LtsMajor, P.Desc LtsMinor]
  where
    go (Entity _ lts) = (ltsMajor lts, ltsMinor lts)

-- | Get the minor version 'y' of latest known LTS snapshot for the major version 'x' in 'lts-x.y'
newestLTSMajor :: GetStackageDatabase env m => Int -> m (Maybe Int)
newestLTSMajor x =
    run $ liftM (fmap $ ltsMinor . entityVal) $ P.selectFirst [LtsMajor P.==. x] [P.Desc LtsMinor]


ltsMajorVersions :: GetStackageDatabase env m => m [(Int, Int)]
ltsMajorVersions =
    run $ liftM (dropOldMinors . map (toPair . entityVal))
        $ P.selectList [] [P.Desc LtsMajor, P.Desc LtsMinor]
  where
    toPair (Lts _ x y) = (x, y)

    dropOldMinors [] = []
    dropOldMinors (l@(x, _):rest) =
        l : dropOldMinors (dropWhile sameMinor rest)
      where
        sameMinor (y, _) = x == y

-- | Look up the date 'in the newest nightly snapshot.
newestNightly :: GetStackageDatabase env m => m (Maybe Day)
newestNightly = run $ liftM (fmap $ nightlyDay . entityVal) $ selectFirst [] [P.Desc NightlyDay]

-- | Get the snapshot which precedes the given one with respect to it's branch (nightly/lts)
snapshotBefore :: GetStackageDatabase env m => SnapName -> m (Maybe (SnapshotId, SnapName))
snapshotBefore (SNLts x y)     = ltsBefore x y
snapshotBefore (SNNightly day) = nightlyBefore day

nightlyBefore :: GetStackageDatabase env m => Day -> m (Maybe (SnapshotId, SnapName))
nightlyBefore day = do
    run $ liftM (fmap go) $ P.selectFirst [NightlyDay P.<. day] [P.Desc NightlyDay]
  where
    go (Entity _ nightly) = (nightlySnap nightly, SNNightly $ nightlyDay nightly)

ltsBefore :: GetStackageDatabase env m => Int -> Int -> m (Maybe (SnapshotId, SnapName))
ltsBefore x y = do
    run $ liftM (fmap go) $ selectFirst
        ( [LtsMajor P.<=. x, LtsMinor P.<. y] P.||.
          [LtsMajor P.<. x]
        )
        [P.Desc LtsMajor, P.Desc LtsMinor]
  where
    go (Entity _ lts) = (ltsSnap lts, SNLts (ltsMajor lts) (ltsMinor lts))


lastLtsNightlyWithoutHoogleDb :: Int -> Int -> RIO StackageCron [(SnapshotId, SnapName)]
lastLtsNightlyWithoutHoogleDb ltsCount nightlyCount = do
    currentHoogleVersionId <- scHoogleVersionId <$> ask
    let getSnapshotsWithoutHoogeDb snapId snapCount =
            map (unValue *** unValue) <$>
            select
                (from $ \(snap `InnerJoin` snapshot) -> do
                     on $ snap ^. snapId ==. snapshot ^. SnapshotId
                     where_ $
                         notExists $
                         from $ \snapshotHoogleDb ->
                             where_ $
                             (snapshotHoogleDb ^. SnapshotHoogleDbSnapshot ==. snapshot ^.
                              SnapshotId) &&.
                             (snapshotHoogleDb ^. SnapshotHoogleDbVersion ==.
                              val currentHoogleVersionId)
                     orderBy [desc (snapshot ^. SnapshotCreated)]
                     limit $ fromIntegral snapCount
                     pure (snapshot ^. SnapshotId, snapshot ^. SnapshotName))
    run $ do
        lts <- getSnapshotsWithoutHoogeDb LtsSnap ltsCount
        nightly <- getSnapshotsWithoutHoogeDb NightlySnap nightlyCount
        pure $ lts ++ nightly


snapshotsJSON :: GetStackageDatabase env m => m A.Value
snapshotsJSON = do
    mlatestNightly <- newestNightly
    ltses <- ltsMajorVersions
    let lts =
            case ltses of
                [] -> []
                majorVersions@(latest:_) -> ("lts" A..= printLts latest) : map toObj majorVersions
        nightly =
            case mlatestNightly of
                Nothing -> id
                Just n  -> (("nightly" A..= printNightly n) :)
    return $ A.object $ nightly lts
  where
    toObj lts@(major, _) = T.pack ("lts-" <> show major) A..= printLts lts
    printLts (major, minor) = "lts-" <> show major <> "." <> show minor
    printNightly day = "nightly-" <> T.pack (show day)


getLatestLtsByGhc :: GetStackageDatabase env m => m [(Int, Int, Text, Day)]
getLatestLtsByGhc =
    run $ fmap (dedupe . map toTuple) $ do
        select $
            from $ \(lts `InnerJoin` snapshot) -> do
                on $ lts ^. LtsSnap ==. snapshot ^. SnapshotId
                orderBy [desc (lts ^. LtsMajor), desc (lts ^. LtsMinor)]
                groupBy
                    ( snapshot ^. SnapshotCompiler
                    , lts ^. LtsId
                    , lts ^. LtsMajor
                    , lts ^. LtsMinor
                    , snapshot ^. SnapshotId)
                return (lts, snapshot)
  where
    toTuple (Entity _ lts, Entity _ snapshot) =
        ( ltsMajor lts
        , ltsMinor lts
        , textDisplay (snapshotCompiler snapshot)
        , snapshotCreated snapshot)
    dedupe []     = []
    dedupe (x:xs) = x : dedupe (dropWhile (\y -> thd x == thd y) xs)
    thd (_, _, x, _) = x

getLatestLtsNameWithHoogle :: GetStackageDatabase env m => m Text
getLatestLtsNameWithHoogle =
    run $ do
        currentHoogleVersionId <- getCurrentHoogleVersionId
        maybe "lts" (textDisplay . unValue) . listToMaybe <$>
            select
                (from $ \(lts `InnerJoin` snapshot `InnerJoin` snapshotHoogleDb) -> do
                     on $ snapshotHoogleDb ^. SnapshotHoogleDbSnapshot ==. snapshot ^. SnapshotId
                     on $ lts ^. LtsSnap ==. snapshot ^. SnapshotId
                     where_ $
                         snapshotHoogleDb ^. SnapshotHoogleDbVersion ==. val currentHoogleVersionId
                     orderBy [desc (lts ^. LtsMajor), desc (lts ^. LtsMinor)]
                     limit 1
                     return (snapshot ^. SnapshotName))

-- | Count snapshots that belong to a specific SnapshotBranch
countSnapshots :: (GetStackageDatabase env m) => Maybe SnapshotBranch -> m Int
countSnapshots Nothing                   = run $ P.count ([] :: [P.Filter Snapshot])
countSnapshots (Just NightlyBranch)      = run $ P.count ([] :: [P.Filter Nightly])
countSnapshots (Just LtsBranch)          = run $ P.count ([] :: [P.Filter Lts])
countSnapshots (Just (LtsMajorBranch x)) = run $ P.count [LtsMajor P.==. x]

-- | Get snapshots that belong to a specific SnapshotBranch
getSnapshots :: (GetStackageDatabase env m)
             => Maybe SnapshotBranch
             -> Int -- ^ limit
             -> Int -- ^ offset
             -> m [Entity Snapshot]
getSnapshots mBranch l o =
    run $
    case mBranch of
        Nothing -> P.selectList [] [P.LimitTo l, P.OffsetBy o, P.Desc SnapshotCreated]
        Just NightlyBranch ->
            select $
            from $ \(nightly `InnerJoin` snapshot) -> do
                on $ nightly ^. NightlySnap ==. snapshot ^. SnapshotId
                orderBy [desc (nightly ^. NightlyDay)]
                limit $ fromIntegral l
                offset $ fromIntegral o
                pure snapshot
        Just LtsBranch -> do
            select $
                from $ \(lts `InnerJoin` snapshot) -> do
                    on $ lts ^. LtsSnap ==. snapshot ^. SnapshotId
                    orderBy [desc (lts ^. LtsMajor), desc (lts ^. LtsMinor)]
                    limit $ fromIntegral l
                    offset $ fromIntegral o
                    pure snapshot
        Just (LtsMajorBranch v) -> do
            select $
                from $ \(lts `InnerJoin` snapshot) -> do
                    on $ lts ^. LtsSnap ==. snapshot ^. SnapshotId
                    orderBy [desc (lts ^. LtsMinor)]
                    where_ ((lts ^. LtsMajor) ==. (val v))
                    limit $ fromIntegral l
                    offset $ fromIntegral o
                    pure snapshot


getSnapshotModules :: GetStackageDatabase env m => SnapshotId -> m [ModuleListingInfo]
getSnapshotModules sid =
    run $ do
        map toModuleListingInfo <$>
            select
                (from $ \(spm `InnerJoin` m `InnerJoin` sp `InnerJoin` pn `InnerJoin` v) -> do
                     on $ sp ^. SnapshotPackageVersion ==. v ^. VersionId
                     on $ sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId
                     on $ spm ^. SnapshotPackageModuleSnapshotPackage ==. sp ^. SnapshotPackageId
                     on $ spm ^. SnapshotPackageModuleModule ==. m ^. ModuleNameId
                     where_ $
                         (sp ^. SnapshotPackageSnapshot ==. val sid) &&.
                         (spm ^. SnapshotPackageModuleHasDocs ==. val True)
                     orderBy [asc (m ^. ModuleNameName), asc (pn ^. PackageNameName)]
                     pure (m ^. ModuleNameName, pn ^. PackageNameName, v ^. VersionVersion))
  where
    toModuleListingInfo (Value moduleName, Value packageName, Value version) =
        ModuleListingInfo
            { mliModuleName = moduleName
            , mliPackageIdentifier = PackageIdentifierP packageName version
            }

getSnapshotPackageModules
    :: SnapshotPackageId
    -> Bool
    -> ReaderT SqlBackend (RIO env) [ModuleNameP]
getSnapshotPackageModules snapshotPackageId hasDocs =
    map unValue <$>
    select
        (from $ \(spm `InnerJoin` m) -> do
             on $ spm ^. SnapshotPackageModuleModule ==. m ^. ModuleNameId
             where_ $
                 (spm ^. SnapshotPackageModuleSnapshotPackage ==. val snapshotPackageId) &&.
                 (spm ^. SnapshotPackageModuleHasDocs ==. val hasDocs)
             orderBy [asc (m ^. ModuleNameName)]
             pure (m ^. ModuleNameName))


getAllPackages :: GetStackageDatabase env m => m [(SnapName, PackageListingInfo)]
getAllPackages =
    run (map toPackageListingInfo <$>
         select
             (from $ \(sp `InnerJoin` snap `InnerJoin` pn `InnerJoin` v) ->
                  distinctOn [don (pn ^. PackageNameName)] $ do
                      on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
                      on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
                      on (sp ^. SnapshotPackageSnapshot ==. snap ^. SnapshotId)
                      orderBy
                          [ asc (pn ^. PackageNameName)
                          , desc (versionArray v)
                          , desc (sp ^. SnapshotPackageRevision)
                          , desc (snap ^. SnapshotCreated)
                          ]
                      pure
                          ( snap ^. SnapshotName
                          , pn ^. PackageNameName
                          , v ^. VersionVersion
                          , sp ^. SnapshotPackageSynopsis
                          , sp ^. SnapshotPackageOrigin)))
  where
    toPackageListingInfo (Value snapName, name, version, synopsis, origin) =
        ( snapName
        , PackageListingInfo
              { pliName = unValue name
              , pliVersion = unValue version
              , pliSynopsis = unValue synopsis
              , pliOrigin = unValue origin
              })

getPackagesForSnapshot :: GetStackageDatabase env m => SnapshotId -> m [PackageListingInfo]
getPackagesForSnapshot snapshotId =
    run (map toPackageListingInfo <$>
         select
             (from $ \(sp `InnerJoin` pn `InnerJoin` v) -> do
                  on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
                  on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
                  where_ (sp ^. SnapshotPackageSnapshot ==. val snapshotId)
                  orderBy [asc (pn ^. PackageNameName)]
                  pure
                      ( pn ^. PackageNameName
                      , v ^. VersionVersion
                      , sp ^. SnapshotPackageSynopsis
                      , sp ^. SnapshotPackageOrigin)))
  where
    toPackageListingInfo (Value pliName, Value pliVersion, Value pliSynopsis, Value pliOrigin) =
        PackageListingInfo {pliName, pliVersion, pliSynopsis, pliOrigin}

getPackagesForSnapshotDiff :: GetStackageDatabase env m => SnapshotId -> m [(PackageNameP, VersionP)]
getPackagesForSnapshotDiff snapshotId =
    run (map toPackageListingInfo <$>
         select
             (from $ \(sp `InnerJoin` pn `InnerJoin` v) -> do
                  on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
                  on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
                  where_ (sp ^. SnapshotPackageSnapshot ==. val snapshotId)
                  orderBy [asc (pn ^. PackageNameName)]
                  pure
                      ( pn ^. PackageNameName
                      , v ^. VersionVersion
                      )))
  where
    toPackageListingInfo (Value name, Value version) = (name, version)


getPackageVersionForSnapshot
  :: GetStackageDatabase env m
  => SnapshotId -> PackageNameP -> m (Maybe VersionP)
getPackageVersionForSnapshot snapshotId pname =
    run $
    selectApplyMaybe
        unValue
        (from $ \(sp `InnerJoin` pn `InnerJoin` v) -> do
             on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
             on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
             where_
                 ((sp ^. SnapshotPackageSnapshot ==. val snapshotId) &&.
                  (pn ^. PackageNameName ==. val pname))
             pure (v ^. VersionVersion))

getLatest ::
       FromPreprocess t
    => PackageNameId
    -> (t -> SqlExpr (Value SnapshotId))
    -> (t -> SqlQuery ())
    -> ReaderT SqlBackend (RIO env) (Maybe SnapshotPackageId)
getLatest pnameid onWhich orderWhich =
    selectApplyMaybe
        unValue
        (from $ \(which `InnerJoin` snap `InnerJoin` sp) -> do
             on (sp ^. SnapshotPackageSnapshot ==. snap ^. SnapshotId)
             on (snap ^. SnapshotId ==. onWhich which)
             where_ (sp ^. SnapshotPackagePackageName ==. val pnameid)
             orderWhich which
             limit 1
             pure (sp ^. SnapshotPackageId))


getLatests :: PackageNameP -> ReaderT SqlBackend (RIO env) [LatestInfo]
getLatests pname = do
    pid <- getPackageNameId $ unPackageNameP pname
    mlatest <- getBy $ UniqueLatestVersion pid
    (mlts, mnightly) <-
      case mlatest of
        Nothing -> do
            mLts <-
                getLatest
                    pid
                    (^. LtsSnap)
                    (\lts -> orderBy [desc (lts ^. LtsMajor), desc (lts ^. LtsMinor)])
            mNightly <-
                getLatest
                    pid
                    (^. NightlySnap)
                    (\nightly -> orderBy [desc (nightly ^. NightlyDay)])
            insert_ LatestVersion
              { latestVersionPackageName = pid
              , latestVersionLts = mLts
              , latestVersionNightly = mNightly
              }
            pure (mLts, mNightly)
        Just (Entity _ (LatestVersion _name mlts mnightly)) -> pure (mlts, mnightly)
    for (catMaybes [mlts, mnightly]) $ \spid -> do
        sp <- maybe (error "impossible") id <$> get spid
        snap <- maybe (error "impossible") id <$> get (snapshotPackageSnapshot sp)
        version <- maybe (error "impossible") id <$> get (snapshotPackageVersion sp)
        pure LatestInfo
          { liSnapName = snapshotName snap
          , liVersionRev = toVersionMRev (versionVersion version) (snapshotPackageRevision sp)
          }

-- | Looks up in pantry the latest information about the package on Hackage.
getHackageLatestVersion ::
       PackageNameP -> ReaderT SqlBackend (RIO env) (Maybe HackageCabalInfo)
getHackageLatestVersion pname =
    selectApplyMaybe toHackageCabalInfo $
    from
        (\(hc `InnerJoin` pn `InnerJoin` v) -> do
             on (hc ^. HackageCabalVersion ==. v ^. VersionId)
             on (hc ^. HackageCabalName ==. pn ^. PackageNameId)
             where_ (pn ^. PackageNameName ==. val pname)
             orderBy [desc (versionArray v), desc (hc ^. HackageCabalRevision)]
             limit 1
             pure
                 ( hc ^. HackageCabalId
                 , hc ^. HackageCabalCabal
                 , v ^. VersionVersion
                 , hc ^. HackageCabalRevision))
  where
    toHackageCabalInfo (cid, cbid, v, rev) =
        HackageCabalInfo
            { hciCabalId = unValue cid
            , hciCabalBlobId = unValue cbid
            , hciPackageName = pname
            , hciVersionRev = toVersionRev (unValue v) (unValue rev)
            }


getSnapshotPackageInfo ::
       GetStackageDatabase env m => SnapName -> PackageNameP -> m (Maybe SnapshotPackageInfo)
getSnapshotPackageInfo snapName pname = run $ getSnapshotPackageInfoQuery snapName pname

getSnapshotPackageInfoQuery ::
       SnapName -> PackageNameP -> ReaderT SqlBackend (RIO env) (Maybe SnapshotPackageInfo)
getSnapshotPackageInfoQuery snapName pname =
    fmap snd . listToMaybe <$>
    (snapshotPackageInfoQuery $ \_sp s pn _v spiQ -> do
         where_ ((s ^. SnapshotName ==. val snapName) &&. (pn ^. PackageNameName ==. val pname))
         pure ((), spiQ))

getSnapshotPackagePageInfoQuery :: SnapshotPackageInfo -> Int -> ReaderT SqlBackend (RIO env) SnapshotPackagePageInfo
getSnapshotPackagePageInfoQuery spi maxDisplayedDeps = do
        mhciLatest <- getHackageLatestVersion $ spiPackageName spi
        -- TODO: check for `spiOrigin spi` once other than `Hackage` are implemented
        forwardDepsCount <- getForwardDepsCount spi
        reverseDepsCount <- getReverseDepsCount spi
        forwardDeps <-
            if forwardDepsCount > 0
                then getForwardDeps spi (Just maxDisplayedDeps)
                else pure []
        reverseDeps <-
            if reverseDepsCount > 0
                then getReverseDeps spi (Just maxDisplayedDeps)
                else pure []
        -- latestInfo <- getLatests (spiPackageName spi)
        let latestInfo = [] -- FIXME!
        moduleNames <- getModuleNames (spiSnapshotPackageId spi)
        mcabalBlobKey <- traverse getBlobKey $ spiCabalBlobId spi
        pure
            SnapshotPackagePageInfo
                { sppiSnapshotPackageInfo = spi
                , sppiLatestHackageCabalInfo = mhciLatest
                , sppiForwardDeps = map (first dropVersionRev) forwardDeps
                , sppiForwardDepsCount = forwardDepsCount
                , sppiReverseDeps = map (first dropVersionRev) reverseDeps
                , sppiReverseDepsCount = reverseDepsCount
                , sppiLatestInfo = latestInfo
                , sppiModuleNames = moduleNames
                , sppiPantryCabal =
                      mcabalBlobKey RIO.<&> \cabalBlobKey ->
                          PantryCabal
                              { pcPackageName = spiPackageName spi
                              , pcVersion = spiVersion spi
                              , pcCabalKey = cabalBlobKey
                              }
                , sppiVersion =
                      listToMaybe
                          [ spiVersionRev spi
                          | VersionRev ver mrev <-
                                maybe [] (pure . hciVersionRev) mhciLatest ++
                                map liVersionRev latestInfo
                          , ver > curVer ||
                                (ver == curVer &&
                                 fromMaybe (Revision 0) mrev > fromMaybe (Revision 0) mcurRev)
                          ]
                }
  where
    VersionRev curVer mcurRev = spiVersionRev spi

getSnapshotPackagePageInfo ::
       GetStackageDatabase env m => SnapshotPackageInfo -> Int -> m SnapshotPackagePageInfo
getSnapshotPackagePageInfo spi maxDisplayedDeps = run $ getSnapshotPackagePageInfoQuery spi maxDisplayedDeps

type SqlExprSPI
     = ( SqlExpr (Value SnapshotPackageId)
       , SqlExpr (Value SnapshotId)
       , SqlExpr (Value SnapName)
       , SqlExpr (Value PackageNameP)
       , SqlExpr (Value (Maybe BlobId))
       , SqlExpr (Value VersionP)
       , SqlExpr (Value (Maybe Revision))
       , SqlExpr (Value Origin)
       , SqlExpr (Value (Maybe TreeEntryId))
       , SqlExpr (Value (Maybe TreeEntryId))
       )

snapshotPackageInfoQuery ::
       (SqlSelect a b)
    => (   SqlExpr (Entity SnapshotPackage)
        -> SqlExpr (Entity Snapshot)
        -> SqlExpr (Entity PackageName)
        -> SqlExpr (Entity Version)
        -> SqlExprSPI
        -> SqlQuery (a, SqlExprSPI)
       )
    -> ReaderT SqlBackend (RIO env) [(b, SnapshotPackageInfo)]
snapshotPackageInfoQuery customize =
    fmap (\(extraValue, spiValues) -> (extraValue, toSnapshotPackageInfo spiValues)) <$>
    select
        (from $ \(sp `InnerJoin` s `InnerJoin` pn `InnerJoin` v) -> do
             on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
             on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
             on (sp ^. SnapshotPackageSnapshot ==. s ^. SnapshotId)
             customize sp s pn v $
                 ( sp ^. SnapshotPackageId
                 , s  ^. SnapshotId
                 , s  ^. SnapshotName
                 , pn ^. PackageNameName
                 , sp ^. SnapshotPackageCabal
                 , v  ^. VersionVersion
                 , sp ^. SnapshotPackageRevision
                 , sp ^. SnapshotPackageOrigin
                 , sp ^. SnapshotPackageReadme
                 , sp ^. SnapshotPackageChangelog))
  where
    toSnapshotPackageInfo (spid, sid, sn, pn, spc, v, spr, spo, rm, cl) =
        SnapshotPackageInfo
            { spiSnapshotPackageId = unValue spid
            , spiSnapshotId = unValue sid
            , spiCabalBlobId = unValue spc
            , spiSnapName = unValue sn
            , spiPackageName = unValue pn
            , spiVersion = unValue v
            , spiRevision = unValue spr
            , spiOrigin = unValue spo
            , spiReadme = unValue rm
            , spiChangelog = unValue cl
            }

getSnapshotPackageLatestVersionQuery ::
       PackageNameP -> ReaderT SqlBackend (RIO env) (Maybe SnapshotPackageInfo)
getSnapshotPackageLatestVersionQuery pname = do
    versions <-
        select $
        from $ \(sp `InnerJoin` pn `InnerJoin` v) -> do
            on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
            on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
            where_ $ pn ^. PackageNameName ==. val pname
            pure (v ^. VersionVersion, sp ^. SnapshotPackageId)
    case L.sortOn Down [(v, spid) | (Value v, Value spid) <- versions] of
        [] -> pure Nothing
        (_, spid):_ ->
            fmap snd . listToMaybe <$>
            (snapshotPackageInfoQuery $ \sp _s _pn _v spiQ -> do
                 where_ $ sp ^. SnapshotPackageId ==. val spid
                 pure ((), spiQ))

getSnapshotPackageLatestVersion ::
       GetStackageDatabase env m
    => PackageNameP
    -> m (Maybe SnapshotPackageInfo)
getSnapshotPackageLatestVersion pname = run (getSnapshotPackageLatestVersionQuery pname)

-- | A helper function that expects at most one element to be returned by a `select` and applies a
-- function to the returned result
selectApplyMaybe ::
       (SqlSelect a b, MonadIO m) => (b -> r) -> SqlQuery a -> ReaderT SqlBackend m (Maybe r)
selectApplyMaybe f = fmap (fmap f . listToMaybe) . select


-- | Convert a string representation of a version to an array so it can be used for sorting.
versionArray :: SqlExpr (Entity Version) -> SqlExpr (Value [Int64])
versionArray v = stringsToInts (stringToArray (v ^. VersionVersion) (val ("." :: String)))

stringsToInts :: SqlExpr (Value [String]) -> SqlExpr (Value [Int64])
stringsToInts = unsafeSqlCastAs "INTEGER[]"

-- | Define postgresql native function in Haskell with Esqueleto
stringToArray ::
       (SqlString s1, SqlString s2)
    => SqlExpr (Value s1)
    -> SqlExpr (Value s2)
    -> SqlExpr (Value [String])
stringToArray s1 s2 = unsafeSqlFunction "string_to_array" (s1, s2)

getSnapshotsForPackage
    :: GetStackageDatabase env m
    => PackageNameP
    -> Maybe Int
    -> m [(CompilerP, SnapshotPackageInfo)]
getSnapshotsForPackage pname mlimit =
    fmap (first unValue) <$>
    run (snapshotPackageInfoQuery $ \_sp s pn _v spiQ -> do
             where_ (pn ^. PackageNameName ==. val pname)
             orderBy [desc (s ^. SnapshotCreated)]
             forM_ mlimit (limit . fromIntegral)
             pure (s ^. SnapshotCompiler, spiQ))


getPackageInfoQuery :: Either HackageCabalInfo SnapshotPackageInfo -> ReaderT SqlBackend (RIO env) PackageInfo
getPackageInfoQuery (Left hci) = do
  cabalBlob <- loadBlobById (hciCabalBlobId hci)
  pure $ toPackageInfo (parseCabalBlob cabalBlob) Nothing Nothing
getPackageInfoQuery (Right spi) = do
    case spiCabalBlobId spi of
        Just cabalBlobId -> do
            gpd <- parseCabalBlob <$> loadBlobById cabalBlobId
            mreadme <- maybe (pure Nothing) getFileByTreeEntryId (spiReadme spi)
            mchangelog <- maybe (pure Nothing) getFileByTreeEntryId (spiChangelog spi)
            pure $
                toPackageInfo
                    gpd
                    (toContentFile Readme <$> mreadme)
                    (toContentFile Changelog <$> mchangelog)
        Nothing -> error "FIXME: handle a case when cabal file isn't available but package.yaml is"
  where
    toContentFile :: (ByteString -> Bool -> a) -> (SafeFilePath, ByteString) -> a
    toContentFile con (path, bs) = con bs (isMarkdownFilePath path)

getPackageInfo ::
       GetStackageDatabase env m => Either HackageCabalInfo SnapshotPackageInfo -> m PackageInfo
getPackageInfo args = run $ getPackageInfoQuery args

getFileByTreeEntryId ::
       TreeEntryId
    -> ReaderT SqlBackend (RIO env) (Maybe (SafeFilePath, ByteString))
getFileByTreeEntryId teid =
    selectApplyMaybe (bimap unValue unValue) $
    from $ \(te `InnerJoin` fp `InnerJoin` b) -> do
        on $ te ^. TreeEntryBlob ==. b ^. BlobId
        on $ te ^. TreeEntryPath ==. fp ^. FilePathId
        where_ $ te ^. TreeEntryId ==. val teid
        pure (fp ^. FilePathPath, b ^. BlobContents)

getModuleNames :: SnapshotPackageId -> ReaderT SqlBackend (RIO env) (Map ModuleNameP Bool)
getModuleNames spid =
    Map.fromList . map (\(md, hs) -> (unValue md, unValue hs)) <$>
    select
        (from $ \(spm `InnerJoin` pm) -> do
             on (spm ^. SnapshotPackageModuleModule ==. pm ^. ModuleNameId)
             where_ (spm ^. SnapshotPackageModuleSnapshotPackage ==. val spid)
             orderBy [desc (pm ^. ModuleNameName)]
             pure (pm ^. ModuleNameName, spm ^. SnapshotPackageModuleHasDocs))

------ Dependencies

getForwardDeps ::
       SnapshotPackageInfo
    -> Maybe Int
    -> ReaderT SqlBackend (RIO env) [(PackageVersionRev, VersionRangeP)]
getForwardDeps spi mlimit =
    fmap toDepRange <$>
    select
        (from $ \(user `InnerJoin` uses `InnerJoin` pn `InnerJoin` v) -> do
             on (uses ^. SnapshotPackageVersion ==. v ^. VersionId)
             on (uses ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
             on (user ^. DepUses ==. uses ^. SnapshotPackagePackageName)
             where_ $
                 (user ^. DepUser ==. val (spiSnapshotPackageId spi)) &&.
                 (uses ^. SnapshotPackageSnapshot ==. val (spiSnapshotId spi))
             orderBy [asc (pn ^. PackageNameName)]
             maybe (pure ()) (limit . fromIntegral) mlimit
             pure
                 ( pn ^. PackageNameName
                 , v ^. VersionVersion
                 , uses ^. SnapshotPackageRevision
                 , user ^. DepRange))
  where
    toDepRange (pn, v, rev, range) =
        (PackageVersionRev (unValue pn) (toVersionMRev (unValue v) (unValue rev)), unValue range)


getForwardDepsCount :: SnapshotPackageInfo -> ReaderT SqlBackend (RIO env) Int
getForwardDepsCount spi = P.count [DepUser P.==. spiSnapshotPackageId spi]

getReverseDepsCount :: SnapshotPackageInfo -> ReaderT SqlBackend (RIO env) Int
getReverseDepsCount spi =
    fromMaybe 0 <$>
    selectApplyMaybe unValue
        (from $ \(sp `InnerJoin` dep `InnerJoin` curPn) -> do
             on (dep ^. DepUses ==. curPn ^. PackageNameId)
             on (sp ^. SnapshotPackageId ==. dep ^. DepUser)
             where_ $
                 (curPn ^. PackageNameName ==. val (spiPackageName spi)) &&.
                 (sp ^. SnapshotPackageSnapshot ==. val (spiSnapshotId spi))
             pure countRows)

getDepsCount :: GetStackageDatabase env m => SnapshotPackageInfo -> m (Int, Int)
getDepsCount spi =
    run $
    (,) <$> getForwardDepsCount spi <*>
    getReverseDepsCount spi

getReverseDeps ::
       SnapshotPackageInfo
    -> Maybe Int -- ^ Optionally limit number of dependencies
    -> ReaderT SqlBackend (RIO env) [(PackageVersionRev, VersionRangeP)]
getReverseDeps spi mlimit =
    fmap toDepRange <$>
    select
        (from $ \(sp `InnerJoin` dep `InnerJoin` pn `InnerJoin` v `InnerJoin` curPn) -> do
             on (dep ^. DepUses ==. curPn ^. PackageNameId)
             on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
             on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
             on (sp ^. SnapshotPackageId ==. dep ^. DepUser)
             where_ $
                 (curPn ^. PackageNameName ==. val (spiPackageName spi)) &&.
                 (sp ^. SnapshotPackageSnapshot ==. val (spiSnapshotId spi))
             orderBy [asc (pn ^. PackageNameName)]
             maybe (pure ()) (limit . fromIntegral) mlimit
             pure
                 ( pn ^. PackageNameName
                 , v ^. VersionVersion
                 , sp ^. SnapshotPackageRevision
                 , dep ^. DepRange))
  where
    toDepRange (pn, v, rev, range) =
        (PackageVersionRev (unValue pn) (toVersionMRev (unValue v) (unValue rev)), unValue range)




----- Deprecated

getDeprecatedQuery :: PackageNameP -> ReaderT SqlBackend (RIO env) (Bool, [PackageNameP])
getDeprecatedQuery pname =
    lookupPackageNameId pname >>= \case
        Just pnid ->
            P.getBy (UniqueDeprecated pnid) >>= \case
                Just (Entity _ (Deprecated _ inFavourOfIds)) -> do
                    names <- mapM lookupPackageNameById inFavourOfIds
                    return (True, catMaybes names)
                Nothing -> return defRes
        Nothing -> return defRes
  where
    defRes = (False, [])

-- | See if a package is deprecated on hackage and in favour of which packages.
getDeprecated :: GetStackageDatabase env m => PackageNameP -> m (Bool, [PackageNameP])
getDeprecated pname = run $ getDeprecatedQuery pname



--------------------------
-- Cron related queries --
--------------------------


snapshotMarkUpdated :: GetStackageDatabase env m => SnapshotId -> UTCTime -> m ()
snapshotMarkUpdated snapKey updatedOn =
    run $ P.update snapKey [SnapshotUpdatedOn P.=. Just updatedOn]

insertSnapshotName :: GetStackageDatabase env m => SnapshotId -> SnapName -> m ()
insertSnapshotName snapKey snapName =
    run $
    case snapName of
        SNLts major minor -> void $ insertUnique $ Lts snapKey major minor
        SNNightly day     -> void $ insertUnique $ Nightly snapKey day

-- | Add a map of all dependencies for the package together with version bounds. Returns a set of
-- all dependencies that could not be found in pantry
insertDeps ::
       HasLogFunc env
    => PackageIdentifierP -- ^ For error reporting only.
    -> SnapshotPackageId
    -> Map PackageNameP VersionRangeP
    -> ReaderT SqlBackend (RIO env) (Set PackageNameP)
insertDeps pid snapshotPackageId dependencies =
    Map.keysSet <$> Map.traverseMaybeWithKey insertDep dependencies
  where
    insertDep dep range =
        lookupPackageNameId dep >>= \case
            Just packageNameId -> do
                void $ insertBy (Dep snapshotPackageId packageNameId range)
                return Nothing
            Nothing -> do
                lift $
                    logWarn $
                    "Couldn't find a dependency of " <> display pid <> " in Pantry with name: " <>
                    display dep
                return $ Just dep

data CabalFileIds = CabalFileIds
    { cfiPackageNameId :: !PackageNameId
    , cfiVersionId :: !VersionId
    , cfiCabalBlobId :: !(Maybe BlobId)
    , cfiModuleNameIds :: ![ModuleNameId]
    }

getCabalFileIds ::
       HasLogFunc env
    => BlobId
    -> GenericPackageDescription
    -> ReaderT SqlBackend (RIO env) CabalFileIds
getCabalFileIds cabalBlobId gpd = do
    let PackageIdentifier name ver = package (packageDescription gpd)
    packageNameId <- getPackageNameId name
    versionId <- getVersionId ver
    moduleNameIds <- mapM insertModuleSafe (extractModuleNames gpd)
    pure
        CabalFileIds
            { cfiPackageNameId = packageNameId
            , cfiVersionId = versionId
            , cfiCabalBlobId = Just cabalBlobId
            , cfiModuleNameIds = moduleNameIds
            }

addCabalFile ::
       HasLogFunc env
    => PackageIdentifierP
    -> ByteString
    -> ReaderT SqlBackend (RIO env) (Maybe (GenericPackageDescription, CabalFileIds))
addCabalFile pid cabalBlob = do
    mgpd <- lift $ parseCabalBlobMaybe pid cabalBlob
    forM mgpd $ \gpd -> do
        (cabalBlobId, _) <- storeBlob cabalBlob
        cabalIds <- getCabalFileIds cabalBlobId gpd
        pure (gpd, cabalIds)

getPackageIds ::
       GenericPackageDescription
    -> Either CabalFileIds (Entity Tree)
    -> ReaderT SqlBackend (RIO env) (CabalFileIds, Maybe (TreeId, BlobId))
getPackageIds gpd =
    \case
        Left cabalFileIds -> pure (cabalFileIds, Nothing)
        Right (Entity treeId tree)
            -- -- TODO: Remove Maybe from cfiCabalBlobId and
            -- --       Generate cabal file from package.yaml:
            -- case treeCabal tree of
            --   Just cabalBlobId -> pure cabalBlobId
            --   Nothing -> do
            --     let rawMetaData = RawPackageMetadata {
            --           rpmName = Just pname
            --           , rpmVersion = Just pver
            --           , rpmTreeKey = treeKey tree
            --           }
            --         rpli = ... get
            --     generateHPack (RPLIArchive / RPLIRepo ..) treeId treeVersion tree
            --     ...
         -> do
            moduleNameIds <- mapM insertModuleSafe (extractModuleNames gpd)
            let cabalFileIds =
                    CabalFileIds
                        { cfiPackageNameId = treeName tree
                        , cfiVersionId = treeVersion tree
                        , cfiCabalBlobId = treeCabal tree
                        , cfiModuleNameIds = moduleNameIds
                        }
            pure (cabalFileIds, Just (treeId, treeKey tree))

-- TODO: Optimize, whenever package is already in one snapshot only create the modules and new
-- SnapshotPackage
addSnapshotPackage ::
       HasLogFunc env
    => SnapshotId
    -> CompilerP
    -> Origin
    -> Either CabalFileIds (Entity Tree)
    -> Maybe HackageCabalId
    -> Bool
    -> Map FlagNameP Bool
    -> PackageIdentifierP
    -> GenericPackageDescription
    -> ReaderT SqlBackend (RIO env) ()
addSnapshotPackage snapshotId compiler origin eCabalTree mHackageCabalId isHidden flags pid gpd = do
    (CabalFileIds{..}, mTree) <- getPackageIds gpd eCabalTree
    let mTreeId = fst <$> mTree
    mrevision <- maybe (pure Nothing) getHackageRevision mHackageCabalId
    mreadme <- fromMaybe (pure Nothing) $ getContentTreeEntryId <$> mTreeId <*> mreadmeQuery
    mchangelog <- fromMaybe (pure Nothing) $ getContentTreeEntryId <$> mTreeId <*> mchangelogQuery
    let snapshotPackage =
            SnapshotPackage
                { snapshotPackageSnapshot = snapshotId
                , snapshotPackagePackageName = cfiPackageNameId
                , snapshotPackageVersion = cfiVersionId
                , snapshotPackageRevision = mrevision
                , snapshotPackageCabal = cfiCabalBlobId
                , snapshotPackageTreeBlob = snd <$> mTree
                , snapshotPackageOrigin = origin
                , snapshotPackageOriginUrl = "" -- TODO: add
                , snapshotPackageSynopsis = getSynopsis gpd
                , snapshotPackageReadme = mreadme
                , snapshotPackageChangelog = mchangelog
                , snapshotPackageIsHidden = isHidden
                , snapshotPackageFlags = flags
                }
        checkForDuplicate =
            \case
                Right key -> pure $ Just key
                Left entity
                    -- Make sure this package still comes from the same place and update
                    -- all the fields to newest values. Necessary for making sure global
                    -- hints do not overwrite hackage packages, but still allows for
                    -- updating package info in case of a forceful update.
                    | snapshotPackageOrigin (entityVal entity) == origin -> do
                        P.replace (entityKey entity) snapshotPackage
                        pure $ Just (entityKey entity)
                _ -> pure Nothing
    msnapshotPackageId <- checkForDuplicate =<< P.insertBy snapshotPackage
    forM_ msnapshotPackageId $ \snapshotPackageId -> do
        _ <- insertDeps pid snapshotPackageId (extractDependencies compiler flags gpd)
        -- TODO: collect all missing dependencies and make a report
        forM_ cfiModuleNameIds $ \modNameId -> do
            void $ P.insertBy (SnapshotPackageModule snapshotPackageId modNameId False)

getContentTreeEntryId ::
       TreeId
    -> (SqlExpr (Value SafeFilePath) -> SqlExpr (Value Bool))
    -> ReaderT SqlBackend (RIO env) (Maybe TreeEntryId)
getContentTreeEntryId treeId filePathQuery = do
    (mteid, _isMarkdown) <- foldl' preferMarkdown (Nothing, False) <$>
      select
          (from $ \(te `InnerJoin` p) -> do
               on $ te ^. TreeEntryPath ==. p ^. FilePathId
               where_ $ (te ^. TreeEntryTree ==. val treeId) &&. filePathQuery (p ^. FilePathPath)
               pure (p ^. FilePathPath, te ^. TreeEntryId))
    pure mteid
  where preferMarkdown (_, False) (Value path, Value teid) = (Just teid, isMarkdownFilePath path)
        preferMarkdown pref@(_, True) _ = pref

mchangelogQuery :: Maybe (SqlExpr (Value SafeFilePath) -> SqlExpr (Value Bool))
mchangelogQuery = do
  changelog <- mkSafeFilePath "changelog."
  changes <- mkSafeFilePath "changes."
  pure (\ path -> (path `ilike` val changelog ++. (%)) ||. (path `ilike` val changes  ++. (%)))

mreadmeQuery :: Maybe (SqlExpr (Value SafeFilePath) -> SqlExpr (Value Bool))
mreadmeQuery = do
  readme <- mkSafeFilePath "readme."
  pure (\ path -> path `ilike` val readme ++. (%))

getHackageRevision :: MonadIO m => HackageCabalId -> ReaderT SqlBackend m (Maybe Revision)
getHackageRevision hcid =
    selectApplyMaybe unValue $
    from $ \hc -> do
        where_ (hc ^. HackageCabalId ==. val hcid)
        pure (hc ^. HackageCabalRevision)


lookupPackageNameId :: PackageNameP -> ReaderT SqlBackend (RIO env) (Maybe PackageNameId)
lookupPackageNameId pname = fmap entityKey <$> getBy (UniquePackageName pname)


lookupPackageNameById :: PackageNameId -> ReaderT SqlBackend (RIO env) (Maybe PackageNameP)
lookupPackageNameById pnid = fmap PackageNameP <$> getPackageNameById pnid

-- | Add or updates package deprecation and its "in favor" list. Returns the Id if package
-- was found in pantry.
addDeprecated :: HasLogFunc env => Deprecation -> ReaderT SqlBackend (RIO env) ()
addDeprecated (Deprecation pname inFavourOfNameSet) = do
    mPackageNameId <- lookupPackageNameId pname
    case mPackageNameId of
        Just packageNameId -> do
            let inFavourOfNames = Set.toList inFavourOfNameSet
            inFavourOfAllIds <- mapM lookupPackageNameId inFavourOfNames
            let (badNames, inFavourOfIds) =
                    partitionEithers $
                    L.zipWith
                        (\name mid -> maybe (Left name) Right mid)
                        inFavourOfNames
                        inFavourOfAllIds
            void $
                upsertBy
                    (UniqueDeprecated packageNameId)
                    (Deprecated packageNameId inFavourOfIds)
                    [DeprecatedInFavourOf P.=. inFavourOfIds]
            unless (null badNames) $
                lift $
                logError $
                mconcat
                    ("Couldn't find in Pantry names of packages in deprecation list: " :
                     L.intersperse ", " (map display badNames))
        Nothing ->
            lift $
            logError $
            "Package name: " <> display pname <> " from deprecation list was not found in Pantry."

-- | In a single transaction clear out all deprecatons and add the new ones.
setDeprecations :: GetStackageDatabase env m => [Deprecation] -> m ()
setDeprecations deprecations = run $ do
  delete $ from $ \(_deprecation :: SqlExpr (Entity Deprecated)) -> pure ()
  mapM_ addDeprecated deprecations


getHackageCabalByRev0 ::
       PackageIdentifierP
    -> ReaderT SqlBackend (RIO env) (Maybe (HackageCabalId, BlobId, Maybe TreeId))
getHackageCabalByRev0 pid = getHackageCabalByRev pid Nothing

getHackageCabalByRev ::
       PackageIdentifierP
    -> Maybe Revision
    -> ReaderT SqlBackend (RIO env) (Maybe (HackageCabalId, BlobId, Maybe TreeId))
getHackageCabalByRev (PackageIdentifierP pname ver) mrev =
    selectApplyMaybe (\(Value hcid, Value bid, Value mtid) -> (hcid, bid, mtid)) $
    from $ \(hc `InnerJoin` pn `InnerJoin` v) -> do
        on (hc ^. HackageCabalVersion ==. v ^. VersionId)
        on (hc ^. HackageCabalName ==. pn ^. PackageNameId)
        where_
            ((pn ^. PackageNameName ==. val pname) &&. (v ^. VersionVersion ==. val ver) &&.
             (hc ^. HackageCabalRevision ==. val (fromMaybe (Revision 0) mrev)))
        return (hc ^. HackageCabalId, hc ^. HackageCabalCabal, hc ^. HackageCabalTree)

-- | This query will return `Nothing` if the tarball for the hackage cabal file hasn't been loaded
-- yet.
getHackageCabalByKey ::
       PackageIdentifierP
    -> BlobKey
    -> ReaderT SqlBackend (RIO env) (Maybe (HackageCabalId, Maybe TreeId))
getHackageCabalByKey (PackageIdentifierP pname ver) (BlobKey sha size) =
    selectApplyMaybe (\(Value hcid, Value mtid) -> (hcid, mtid)) $
    from $ \(hc `InnerJoin` pn `InnerJoin` v `InnerJoin` b) -> do
        on (hc ^. HackageCabalCabal ==. b ^. BlobId)
        on (hc ^. HackageCabalVersion ==. v ^. VersionId)
        on (hc ^. HackageCabalName ==. pn ^. PackageNameId)
        where_
            ((pn ^. PackageNameName ==. val pname) &&. (v ^. VersionVersion ==. val ver) &&.
             (b ^. BlobSha ==. val sha) &&.
             (b ^. BlobSize ==. val size))
        return (hc ^. HackageCabalId, hc ^. HackageCabalTree)


getSnapshotPackageId ::
       SnapshotId
    -> PackageIdentifierP
    -> ReaderT SqlBackend (RIO env) (Maybe SnapshotPackageId)
getSnapshotPackageId snapshotId (PackageIdentifierP pname ver) =
    selectApplyMaybe unValue $
    from $ \(sp `InnerJoin` pn `InnerJoin` v) -> do
        on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
        on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
        where_
            ((sp ^. SnapshotPackageSnapshot ==. val snapshotId) &&.
             (pn ^. PackageNameName ==. val pname) &&.
             (v ^. VersionVersion ==. val ver))
        return (sp ^. SnapshotPackageId)


getSnapshotPackageCabalBlob ::
       GetStackageDatabase env m => SnapshotId -> PackageNameP -> m (Maybe ByteString)
getSnapshotPackageCabalBlob snapshotId pname =
    run $ selectApplyMaybe unValue $
    from $ \(blob `InnerJoin` sp `InnerJoin` pn) -> do
        on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
        on (just (blob ^. BlobId) ==. sp ^. SnapshotPackageCabal)
        where_
            ((sp ^. SnapshotPackageSnapshot ==. val snapshotId) &&.
             (pn ^. PackageNameName ==. val pname))
        return (blob ^. BlobContents)

-- | Idempotent and thread safe way of adding a new module.
insertModuleSafe :: ModuleNameP -> ReaderT SqlBackend (RIO env) ModuleNameId
insertModuleSafe modName = do
    rawExecute
        "INSERT INTO module_name(name) VALUES (?) ON CONFLICT DO NOTHING"
        [toPersistValue modName]
    mModId <-
        select $
        from $ \m -> do
            where_ (m ^. ModuleNameName ==. val modName)
            return (m ^. ModuleNameId)
    case mModId of
        [Value modId] -> return modId
        _ -> error $ "Module name: " ++ show modName ++ " should have been inserted by now"


markModuleHasDocs ::
       SnapshotId
    -> PackageIdentifierP
    -> Maybe SnapshotPackageId
    -- ^ If we know ahead of time the SnapshotPackageId it will speed things up, since we don't have
    -- to look it up in the database.
    -> ModuleNameP
    -> ReaderT SqlBackend (RIO env) (Maybe SnapshotPackageId)
markModuleHasDocs snapshotId pid mSnapshotPackageId modName =
    maybe (getSnapshotPackageId snapshotId pid) (pure . Just) mSnapshotPackageId >>= \case
        Just snapshotPackageId -> do
            rawExecute
                "UPDATE snapshot_package_module \
                \SET has_docs = true \
                \FROM module_name \
                \WHERE module_name.id = snapshot_package_module.module \
                \AND module_name.name = ? \
                \AND snapshot_package_module.snapshot_package = ?"
                [toPersistValue modName, toPersistValue snapshotPackageId]
            return $ Just snapshotPackageId
        Nothing -> return Nothing


-- | We can either check or insert hoogle db for current hoogle version for current
-- snapshot. Returns True if current hoogle version was not in the database.
checkInsertSnapshotHoogleDb :: Bool -> SnapshotId -> RIO StackageCron Bool
checkInsertSnapshotHoogleDb shouldInsert snapshotId = do
    hoogleVersionId <- scHoogleVersionId <$> ask
    let sh = SnapshotHoogleDb snapshotId hoogleVersionId
    run $
        if shouldInsert
            then do
                mhver <-
                    (fmap unValue . listToMaybe) <$>
                    select
                        (from
                             (\v -> do
                                  where_ $ v ^. VersionId ==. val hoogleVersionId
                                  pure (v ^. VersionVersion)))
                forM_ mhver $ \hver ->
                    lift $
                    logInfo $
                    "Marking hoogle database for version " <> display hver <> " as available."
                isJust <$> P.insertUniqueEntity sh
            else isJust <$> P.checkUnique sh
