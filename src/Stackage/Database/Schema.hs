{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module Stackage.Database.Schema
    ( -- * Database
      run
    , runDatabase
    , StackageDatabase
    , GetStackageDatabase(..)
    , withStackageDatabase
    , runStackageMigrations
    , runStackageMigrations'
    , getCurrentHoogleVersionId
    , getCurrentHoogleVersionIdWithPantryConfig
    -- * Tables
    , Unique(..)
    , EntityField(..)
    -- ** Snapshot
    , Snapshot(..)
    , SnapshotId
    , SnapshotHoogleDb(..)
    , Lts(..)
    , Nightly(..)
    -- ** Package
    , SnapshotPackage(..)
    , SnapshotPackageId
    , SnapshotPackageModule(..)
    , SnapshotPackageModuleId
    , Dep(..)
    , DepId
    , Deprecated(..)
    , DeprecatedId
    -- ** Pantry
    , module PS
    ) where

import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
#if MIN_VERSION_monad_logger(0,3,10) && MIN_VERSION_persistent_postgresql(2,12,0)
import Control.Monad.Logger (MonadLoggerIO)
#else
import Control.Monad.Logger (MonadLogger)
#endif
import qualified Data.Aeson as A
import Data.Pool (destroyAllResources, Pool)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Sqlite (createSqlitePool)
import Database.Persist.TH
import Pantry (HasPantryConfig(..), Revision, parseVersionThrowing)
import Pantry.Internal.Stackage as PS (BlobId, HackageCabalId, ModuleNameId,
                                       PackageNameId, Tree(..),
                                       TreeEntryId, TreeId, Unique(..),
                                       VersionId, unBlobKey)
import Pantry.Internal.Stackage (PantryConfig(..), Storage(..), getVersionId)
import qualified Pantry.Internal.Stackage as Pantry (migrateAll)
import RIO
import RIO.Time
import Types (CompilerP(..), FlagNameP, Origin, SnapName, VersionRangeP)
import Settings (DatabaseSettings (..))
import UnliftIO.Concurrent (getNumCapabilities)

currentSchema :: Int
currentSchema = 1

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Schema
    val Int
    deriving Show

Snapshot
    name SnapName
    compiler CompilerP
    created Day
    updatedOn UTCTime Maybe
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
SnapshotHoogleDb
    snapshot SnapshotId
    version VersionId
    UniqueSnapshotHoogleVersion snapshot version
SnapshotPackage
    snapshot SnapshotId
    packageName PackageNameId
    version VersionId
    revision Revision Maybe
    cabal BlobId Maybe
    treeBlob BlobId Maybe
    origin Origin
    originUrl Text
    synopsis Text
    readme TreeEntryId Maybe
    changelog TreeEntryId Maybe
    isHidden Bool -- used for pantry, but is not relevant for stackage
    flags (Map FlagNameP Bool)
    UniqueSnapshotPackage snapshot packageName
SnapshotPackageModule
    snapshotPackage SnapshotPackageId
    module ModuleNameId
    hasDocs Bool
    UniqueSnapshotPackageModule snapshotPackage module
Dep
    user SnapshotPackageId
    uses PackageNameId
    range VersionRangeP
    UniqueDep user uses
Deprecated
    package PackageNameId
    inFavourOf [PackageNameId]
    UniqueDeprecated package
|]

_hideUnusedWarnings :: (SchemaId, LtsId, NightlyId, SnapshotHoogleDbId) -> ()
_hideUnusedWarnings _ = ()


instance A.ToJSON Snapshot where
  toJSON Snapshot{..} =
    A.object [ "name"     A..= snapshotName
             , "ghc"      A..= ghc -- TODO: deprecate? since it's encapsulated in `compiler`
             , "compiler" A..= snapshotCompiler
             , "created"  A..= formatTime defaultTimeLocale "%F" snapshotCreated
             ]
    where CompilerGHC ghc = snapshotCompiler


newtype StackageDatabase = StackageDatabase
    { _runDatabase :: forall env a . HasLogFunc env =>
                                       ReaderT SqlBackend (RIO env) a -> (RIO env) a
    }

runDatabase ::
       forall env a. HasLogFunc env
    => StackageDatabase
    -> ReaderT SqlBackend (RIO env) a
    -> (RIO env) a
runDatabase = _runDatabase

class (MonadThrow m, MonadIO m) => GetStackageDatabase env m | m -> env where
    getStackageDatabase :: m StackageDatabase

    getLogFunc :: m RIO.LogFunc


instance (HasLogFunc env, HasPantryConfig env) => GetStackageDatabase env (RIO env) where
    getStackageDatabase = view pantryConfigL >>= getStackageDatabaseFromPantry
    getLogFunc = view logFuncL

getStackageDatabaseFromPantry :: PantryConfig -> RIO env StackageDatabase
getStackageDatabaseFromPantry pc = do
    let Storage runStorage _ = pcStorage pc
    pure $ StackageDatabase runStorage


getCurrentHoogleVersionId :: HasLogFunc env => ReaderT SqlBackend (RIO env) VersionId
getCurrentHoogleVersionId = do
    currentHoogleVersion <- parseVersionThrowing VERSION_hoogle
    getVersionId currentHoogleVersion

getCurrentHoogleVersionIdWithPantryConfig :: HasLogFunc env => PantryConfig -> RIO env VersionId
getCurrentHoogleVersionIdWithPantryConfig pantryConfig = do
    stackageDb <- getStackageDatabaseFromPantry pantryConfig
    runDatabase stackageDb getCurrentHoogleVersionId


run :: GetStackageDatabase env m => SqlPersistT (RIO RIO.LogFunc) a -> m a
run inner = do
    stackageDatabase <- getStackageDatabase
    logFunc <- getLogFunc
    runRIO logFunc $ runDatabase stackageDatabase inner


withStackageDatabase :: MonadUnliftIO m => Bool -> DatabaseSettings -> (StackageDatabase -> m a) -> m a
withStackageDatabase shouldLog dbs inner = do
    let
#if MIN_VERSION_monad_logger(0,3,10) && MIN_VERSION_persistent_postgresql(2,12,0)
        makePool :: (MonadUnliftIO m, MonadLoggerIO m) => m (Pool SqlBackend)
#else
        makePool :: (MonadUnliftIO m, MonadLogger   m) => m (Pool SqlBackend)
#endif
        makePool =
            case dbs of
                DSPostgres connStr mSize -> do
                  size <- maybe getNumCapabilities pure mSize
                  createPostgresqlPool (encodeUtf8 connStr) size
                DSSqlite connStr size -> do
                    pool <- createSqlitePool connStr size
                    runSqlPool (do
                        runMigration Pantry.migrateAll
                        runMigration migrateAll
                        ) pool
                    pure pool
        getPoolIO =
            if shouldLog
                then runStdoutLoggingT makePool
                else runNoLoggingT makePool
    bracket (liftIO getPoolIO) (liftIO . destroyAllResources) $ \pool -> do
        inner (StackageDatabase (`runSqlPool` pool))

getSchema :: ReaderT SqlBackend (RIO RIO.LogFunc) (Maybe Int)
getSchema =
    do
        eres <- tryAny (selectList [] [])
        lift $ logInfo $ "getSchema result: " <> displayShow eres
        case eres of
            Right [Entity _ (Schema v)] -> return $ Just v
            _                           -> return Nothing

runStackageMigrations' :: PantryConfig -> RIO RIO.LogFunc () -- HasLogFunc env => PantryConfig -> RIO env ()
runStackageMigrations' pantryConfig = do
    stackageDb <- getStackageDatabaseFromPantry pantryConfig
    runDatabase stackageDb stackageMigrations


runStackageMigrations :: (HasLogFunc env, GetStackageDatabase env (RIO env)) => RIO env ()
runStackageMigrations = run stackageMigrations

stackageMigrations :: ReaderT SqlBackend (RIO RIO.LogFunc) () -- ReaderT SqlBackend (RIO RIO.LogFunc) ()
stackageMigrations = do
    runMigration Pantry.migrateAll
    runMigration migrateAll
    actualSchema <- getSchema
    unless (actualSchema == Just currentSchema) $ do
        lift $
            logWarn $
            "Current schema does not match actual schema: " <>
            displayShow (actualSchema, currentSchema)
        deleteWhere ([] :: [Filter Schema])
        insert_ $ Schema currentSchema
