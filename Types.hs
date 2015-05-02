module Types where

import ClassyPrelude.Yesod
import Data.Aeson
import Data.BlobStore (ToPath (..), BackupToS3 (..))
import Data.Hashable (hashUsing)
import Text.Blaze (ToMarkup)
import Database.Persist.Sql (PersistFieldSql (sqlType))
import qualified Data.Text as T

newtype PackageName = PackageName { unPackageName :: Text }
    deriving (Show, Read, Typeable, Eq, Ord, Hashable, PathPiece, ToMarkup, PersistField, IsString)
instance PersistFieldSql PackageName where
    sqlType = sqlType . liftM unPackageName
newtype Version = Version { unVersion :: Text }
    deriving (Show, Read, Typeable, Eq, Ord, Hashable, PathPiece, ToMarkup, PersistField)
instance PersistFieldSql Version where
    sqlType = sqlType . liftM unVersion
newtype PackageSetIdent = PackageSetIdent { unPackageSetIdent :: Text }
    deriving (Show, Read, Typeable, Eq, Ord, Hashable, PathPiece, ToMarkup, PersistField)
instance PersistFieldSql PackageSetIdent where
    sqlType = sqlType . liftM unPackageSetIdent

data PackageNameVersion = PNVTarball !PackageName !Version
                        | PNVNameVersion !PackageName !Version
                        | PNVName !PackageName
    deriving (Show, Read, Typeable, Eq, Ord)

instance PathPiece PackageNameVersion where
    toPathPiece (PNVTarball x y) = concat [toPathPiece x, "-", toPathPiece y, ".tar.gz"]
    toPathPiece (PNVNameVersion x y) = concat [toPathPiece x, "-", toPathPiece y]
    toPathPiece (PNVName x) = toPathPiece x
    fromPathPiece t' | Just t <- stripSuffix ".tar.gz" t' =
        case T.breakOnEnd "-" t of
            ("", _) -> Nothing
            (_, "") -> Nothing
            (T.init -> name, version) -> Just $ PNVTarball (PackageName name) (Version version)
    fromPathPiece t = Just $
        case T.breakOnEnd "-" t of
            ("", _) -> PNVName (PackageName t)
            (T.init -> name, version) | validVersion version ->
                PNVNameVersion (PackageName name) (Version version)
            _ -> PNVName (PackageName t)
      where
        validVersion =
            all f
          where
            f c = (c == '.') || ('0' <= c && c <= '9')

data StoreKey = HackageCabal !PackageName !Version
              | HackageSdist !PackageName !Version
              | CabalIndex !PackageSetIdent
              | CustomSdist !PackageSetIdent !PackageName !Version
              | SnapshotBundle !PackageSetIdent
              | HaddockBundle !PackageSetIdent
              | HoogleDB !PackageSetIdent !HoogleVersion
    deriving (Show, Eq, Ord, Typeable)

newtype HoogleVersion = HoogleVersion Text
    deriving (Show, Eq, Ord, Typeable, PathPiece)
currentHoogleVersion :: HoogleVersion
currentHoogleVersion = HoogleVersion VERSION_hoogle

instance ToPath StoreKey where
    toPath (HackageCabal name version) = ["hackage", toPathPiece name, toPathPiece version ++ ".cabal"]
    toPath (HackageSdist name version) = ["hackage", toPathPiece name, toPathPiece version ++ ".tar.gz"]
    toPath (CabalIndex ident) = ["cabal-index", toPathPiece ident ++ ".tar.gz"]
    toPath (CustomSdist ident name version) =
        [ "custom-tarball"
        , toPathPiece ident
        , toPathPiece name
        , toPathPiece version ++ ".tar.gz"
        ]
    toPath (SnapshotBundle ident) =
        [ "bundle"
        , toPathPiece ident ++ ".tar.gz"
        ]
    toPath (HaddockBundle ident) =
        [ "haddock"
        , toPathPiece ident ++ ".tar.xz"
        ]
    toPath (HoogleDB ident ver) =
        [ "hoogle"
        , toPathPiece ver
        , toPathPiece ident ++ ".hoo.gz"
        ]
instance BackupToS3 StoreKey where
    shouldBackup HackageCabal{} = False
    shouldBackup HackageSdist{} = False
    shouldBackup CabalIndex{} = True
    shouldBackup CustomSdist{} = True
    shouldBackup SnapshotBundle{} = True
    shouldBackup HaddockBundle{} = True
    shouldBackup HoogleDB{} = True

newtype HackageRoot = HackageRoot { unHackageRoot :: Text }
    deriving (Show, Read, Typeable, Eq, Ord, Hashable, PathPiece, ToMarkup)

class HasHackageRoot a where
    getHackageRoot :: a -> HackageRoot
instance HasHackageRoot HackageRoot where
    getHackageRoot = id

data UnpackStatus = USReady
                  | USBusy
                  | USFailed !Text

data StackageExecutable
    = StackageWindowsExecutable
    | StackageUnixExecutable
    deriving (Show, Read, Eq)

instance PathPiece StackageExecutable where
    -- TODO: distribute stackage, not just stackage-setup
    toPathPiece StackageWindowsExecutable = "stackage-setup.exe"
    toPathPiece StackageUnixExecutable = "stackage-setup"

    fromPathPiece "stackage-setup" = Just StackageUnixExecutable
    fromPathPiece "stackage-setup.exe" = Just StackageWindowsExecutable
    fromPathPiece _ = Nothing

data GhcMajorVersion = GhcMajorVersion Int Int
  deriving (Eq)

ghcMajorVersionToText :: GhcMajorVersion -> Text
ghcMajorVersionToText (GhcMajorVersion a b)
  = pack (show a) <> "." <> pack (show b)

ghcMajorVersionFromText :: Text -> Maybe GhcMajorVersion
ghcMajorVersionFromText t = case T.splitOn "." t of
  [readMay -> Just a, readMay -> Just b] ->
    Just $ GhcMajorVersion a b
  _ -> Nothing

instance PersistFieldSql GhcMajorVersion where
    sqlType = sqlType . liftM ghcMajorVersionToText

instance PersistField GhcMajorVersion where
    toPersistValue = toPersistValue . ghcMajorVersionToText
    fromPersistValue v = do
        t <- fromPersistValueText v
        case ghcMajorVersionFromText t of
            Just ver -> return ver
            Nothing -> Left $ "Cannot convert to GhcMajorVersion: " <> t

instance Hashable GhcMajorVersion where
  hashWithSalt = hashUsing ghcMajorVersionToText

instance FromJSON GhcMajorVersion where
  parseJSON = withText "GhcMajorVersion" $
    maybe mzero return . ghcMajorVersionFromText

instance ToJSON GhcMajorVersion where
  toJSON = toJSON . ghcMajorVersionToText


data SupportedArch
    = Win32
    | Win64
    | Linux32
    | Linux64
    | Mac32
    | Mac64
    deriving (Enum, Bounded, Show, Read, Eq)

instance Hashable SupportedArch where
    hashWithSalt = hashUsing fromEnum

instance PathPiece SupportedArch where
    toPathPiece Win32 = "win32"
    toPathPiece Win64 = "win64"
    toPathPiece Linux32 = "linux32"
    toPathPiece Linux64 = "linux64"
    toPathPiece Mac32 = "mac32"
    toPathPiece Mac64 = "mac64"

    fromPathPiece "win32" = Just Win32
    fromPathPiece "win64" = Just Win64
    fromPathPiece "linux32" = Just Linux32
    fromPathPiece "linux64" = Just Linux64
    fromPathPiece "mac32" = Just Mac32
    fromPathPiece "mac64" = Just Mac64
    fromPathPiece _ = Nothing
