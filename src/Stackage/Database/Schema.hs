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
module Stackage.Database.Schema
    ( -- * Database
      run
    , runDatabase
    , StackageDatabase
    , GetStackageDatabase(..)
    , withStackageDatabase
    , runStackageMigrations
    -- * Tables
    , Unique(..)
    , EntityField(..)
    -- ** Snapshot
    , Snapshot(..)
    , SnapshotId
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
import qualified Data.Aeson as A
import Data.Pool (destroyAllResources)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Pantry (HasPantryConfig(..), Revision)
import Pantry.Internal.Stackage as PS (BlobId, HackageCabalId, ModuleNameId,
                                       PackageNameId, Tree(..), TreeEntry(..),
                                       TreeEntryId, TreeId, Unique(..),
                                       VersionId, unBlobKey)
import Pantry.Internal.Stackage (PantryConfig(..), Storage(..))
import qualified Pantry.Internal.Stackage as Pantry (migrateAll)
import RIO
import RIO.Time
import Types (CompilerP(..), FlagNameP, Origin, SnapName, VersionRangeP)

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

_hideUnusedWarnings :: (SchemaId, LtsId, NightlyId) -> ()
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
    getStackageDatabase = do
      env <- view pantryConfigL
      let Storage runStorage _ = pcStorage env
      pure $ StackageDatabase runStorage
    getLogFunc = view logFuncL



run :: GetStackageDatabase env m => SqlPersistT (RIO RIO.LogFunc) a -> m a
run inner = do
    stackageDatabase <- getStackageDatabase
    logFunc <- getLogFunc
    runRIO logFunc $ runDatabase stackageDatabase inner


withStackageDatabase :: MonadUnliftIO m => Bool -> PostgresConf -> (StackageDatabase -> m a) -> m a
withStackageDatabase shouldLog pg inner = do
    let getPoolIO =
            if shouldLog
                then runStdoutLoggingT $ createPostgresqlPool (pgConnStr pg) (pgPoolSize pg)
                else runNoLoggingT $ createPostgresqlPool (pgConnStr pg) (pgPoolSize pg)
    bracket (liftIO getPoolIO) (liftIO . destroyAllResources) $ \pool ->
        inner (StackageDatabase (`runSqlPool` pool))


getSchema :: (HasLogFunc env, GetStackageDatabase env (RIO env)) => RIO env (Maybe Int)
getSchema =
    run $ do
        eres <- tryAny (selectList [] [])
        lift $ logInfo $ "getSchema result: " <> displayShow eres
        case eres of
            Right [Entity _ (Schema v)] -> return $ Just v
            _                           -> return Nothing

runStackageMigrations :: (HasLogFunc env, GetStackageDatabase env (RIO env)) => RIO env ()
runStackageMigrations = do
    actualSchema <- getSchema
    run $ do
        runMigration Pantry.migrateAll
        runMigration migrateAll
        unless (actualSchema == Just currentSchema) $ do
            lift $
                logWarn $
                "Current schema does not match actual schema: " <>
                displayShow (actualSchema, currentSchema)
            deleteWhere ([] :: [Filter Schema])
            insert_ $ Schema currentSchema
