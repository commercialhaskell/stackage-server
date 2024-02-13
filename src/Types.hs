{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Types
    ( SnapName (..)
    , isLts
    , isNightly
    , SnapshotBranch(..)
    , snapshotPrettyName
    , snapshotPrettyNameShort
    , PackageNameP(..)
    , parsePackageNameP
    , VersionP(..)
    , Revision(..)
    , VersionRev(..)
    , VersionRangeP(..)
    , CompilerP(..)
    , parseCompilerP
    , FlagNameP(..)
    , PackageVersionRev(..)
    , ModuleNameP(..)
    , parseModuleNameP
    , SafeFilePath
    , unSafeFilePath
    , moduleNameFromComponents
    , PackageIdentifierP(..)
    , PackageNameVersion(..)
    , GenericPackageDescription
    , HoogleVersion(..)
    , currentHoogleVersion
    , UnpackStatus(..)
    , GhcMajorVersion(..)
    , GhcMajorVersionFailedParse(..)
    , ghcMajorVersionFromText
    , keepMajorVersion
    , dtDisplay
    , dtParse
    , SupportedArch(..)
    , Year
    , Month(Month)
    , Origin(..)
    ) where

import ClassyPrelude.Yesod (ToBuilder(..))
import Data.Aeson
import Data.Char (ord)
import Data.Hashable (hashUsing, hashWithSalt)
import qualified Data.Text as T
import qualified Data.Text.Read as Reader
import Data.Typeable
import Database.Esqueleto.Internal.Internal
import Database.Persist
import Database.Persist.Sql (PersistFieldSql(sqlType))
import qualified Distribution.ModuleName as DT (components, fromComponents,
                                                validModuleComponent)
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Parsec as DT (Parsec)
import Distribution.Pretty as DT (Pretty)
import qualified Distribution.Text as DT (display, simpleParse)
import Distribution.Types.VersionRange (VersionRange)
import Distribution.Version (mkVersion, versionNumbers)
import Pantry (FlagName, Revision(..), packageNameString, parsePackageName,
               parseVersionThrowing, parseVersion, versionString)
import Pantry.Internal.Stackage (ModuleNameP(..), PackageNameP(..),
                                 SafeFilePath, VersionP(..), unSafeFilePath)
import RIO
import qualified RIO.Map as Map
import RIO.Time (Day)
import Text.Blaze (ToMarkup(..))
import Web.PathPieces

data ParseFailedException = ParseFailedException !TypeRep !String
    deriving (Show, Typeable)
instance Exception ParseFailedException where
    displayException (ParseFailedException tyRep origString) =
        "Was unable to parse " ++ showsTypeRep tyRep ": " ++ origString

dtParse :: forall a m. (Typeable a, DT.Parsec a, MonadThrow m) => Text -> m a
dtParse txt =
    let str = T.unpack txt
     in case DT.simpleParse str of
            Nothing -> throwM $ ParseFailedException (typeRep (Proxy :: Proxy a)) str
            Just dt -> pure dt

dtDisplay :: (DT.Pretty a, IsString b) => a -> b
dtDisplay = fromString . DT.display



data SnapName = SNLts !Int !Int
              | SNNightly !Day
    deriving (Eq, Ord, Read, Show)

isLts :: SnapName -> Bool
isLts SNLts{}     = True
isLts SNNightly{} = False

isNightly :: SnapName -> Bool
isNightly SNLts{}     = False
isNightly SNNightly{} = True


snapshotPrettyName :: SnapName -> CompilerP -> Text
snapshotPrettyName sName sCompiler =
    T.concat [snapshotPrettyNameShort sName, " (", textDisplay sCompiler, ")"]

snapshotPrettyNameShort :: SnapName -> Text
snapshotPrettyNameShort name =
    case name of
        SNLts x y -> T.concat ["LTS Haskell ", T.pack (show x), ".", T.pack (show y)]
        SNNightly d -> "Stackage Nightly " <> T.pack (show d)


instance ToJSONKey SnapName

instance ToJSON SnapName where
    toJSON = String . toPathPiece

instance PersistField SnapName where
    toPersistValue = toPersistValue . toPathPiece
    fromPersistValue v = do
        t <- fromPersistValue v
        case fromPathPiece t of
            Nothing -> Left $ "Invalid SnapName: " <> t
            Just x  -> return x
instance PersistFieldSql SnapName where
    sqlType = sqlType . fmap toPathPiece
instance PathPiece SnapName where
    toPathPiece = textDisplay
    fromPathPiece = parseSnapName

instance FromJSON SnapName where
    parseJSON = withText "SnapName" (maybe (fail "Can't parse snapshot name") pure . parseSnapName)

instance ToMarkup SnapName where
    toMarkup = toMarkup . textDisplay

instance Display SnapName where
    display =
        \case
            (SNLts x y) -> mconcat ["lts-", displayShow x, ".", displayShow y]
            (SNNightly d) -> "nightly-" <> displayShow d

parseSnapName :: Text -> Maybe SnapName
parseSnapName t0 = nightly <|> lts
  where
    nightly = fmap SNNightly $ T.stripPrefix "nightly-" t0 >>= (readMaybe . T.unpack)
    lts = do
        t1 <- T.stripPrefix "lts-" t0
        Right (x, t2) <- Just $ Reader.decimal t1
        t3 <- T.stripPrefix "." t2
        Right (y, "") <- Just $ Reader.decimal t3
        return $ SNLts x y

data SnapshotBranch = LtsMajorBranch Int
                    | LtsBranch
                    | NightlyBranch
                    deriving (Eq, Read, Show)
instance PathPiece SnapshotBranch where
    toPathPiece NightlyBranch      = "nightly"
    toPathPiece LtsBranch          = "lts"
    toPathPiece (LtsMajorBranch x) = "lts-" <> T.pack (show x)

    fromPathPiece "nightly" = Just NightlyBranch
    fromPathPiece "lts" = Just LtsBranch
    fromPathPiece t0 = do
        t1 <- T.stripPrefix "lts-" t0
        Right (x, "") <- Just $ Reader.decimal t1
        Just $ LtsMajorBranch x

newtype PackageSetIdent = PackageSetIdent { unPackageSetIdent :: Text }
    deriving (Show, Read, Typeable, Eq, Ord, Hashable, PathPiece, ToMarkup, PersistField)
instance PersistFieldSql PackageSetIdent where
    sqlType = sqlType . fmap unPackageSetIdent

data PackageNameVersion = PNVTarball !PackageNameP !VersionP
                        | PNVNameVersion !PackageNameP !VersionP
                        | PNVName !PackageNameP
    deriving (Read, Show, Eq, Ord)

data PackageIdentifierP =
    PackageIdentifierP !PackageNameP
                       !VersionP
    deriving (Eq, Ord, Show)

instance Display PackageIdentifierP where
    display (PackageIdentifierP pname ver) = display pname <> "-" <> display ver
instance PathPiece PackageIdentifierP where
    toPathPiece = textDisplay
    fromPathPiece t = do
        let (tName', tVer) = T.breakOnEnd "-" t
        (tName, '-') <- T.unsnoc tName'
        guard $ not (T.null tName || T.null tVer)
        PackageIdentifierP <$> fromPathPiece tName <*> fromPathPiece tVer
instance ToMarkup PackageIdentifierP where
    toMarkup = toMarkup . textDisplay

instance Hashable PackageNameP where
    hashWithSalt = hashUsing textDisplay
instance ToBuilder PackageNameP Builder where
    toBuilder = getUtf8Builder . display

parsePackageNameP :: String -> Maybe PackageNameP
parsePackageNameP = fmap PackageNameP . parsePackageName

instance PathPiece PackageNameP where
    fromPathPiece = parsePackageNameP . T.unpack
    toPathPiece = textDisplay
instance ToMarkup PackageNameP where
    toMarkup = toMarkup . packageNameString . unPackageNameP
instance SqlString PackageNameP

instance SqlString SafeFilePath

instance PathPiece VersionP where
    fromPathPiece = fmap VersionP . parseVersion . T.unpack
    toPathPiece = textDisplay
instance ToMarkup VersionP where
    toMarkup (VersionP v) = toMarkup $ versionString v
instance ToBuilder VersionP Builder where
    toBuilder = getUtf8Builder . display
instance SqlString VersionP

keepMajorVersion :: VersionP -> VersionP
keepMajorVersion pver@(VersionP ver) =
    case versionNumbers ver of
        nums@(_major:_minor:_) -> VersionP (mkVersion nums)
        _                      -> pver


instance ToMarkup Revision where
    toMarkup (Revision r) = "rev:" <> toMarkup r

data VersionRev = VersionRev
    { vrVersion  :: !VersionP
    , vrRevision :: !(Maybe Revision)
    } deriving (Eq, Show)

instance ToMarkup VersionRev where
    toMarkup (VersionRev version mrev) =
        toMarkup version <> maybe "" (("@" <>) . toMarkup) mrev

data PackageVersionRev = PackageVersionRev !PackageNameP !VersionRev deriving (Eq, Show)

instance ToMarkup PackageVersionRev where
    toMarkup (PackageVersionRev pname version) = toMarkup pname <> "-" <> toMarkup version


instance PathPiece PackageNameVersion where
    toPathPiece (PNVTarball x y) = T.concat [toPathPiece x, "-", toPathPiece y, ".tar.gz"]
    toPathPiece (PNVNameVersion x y) = T.concat [toPathPiece x, "-", toPathPiece y]
    toPathPiece (PNVName x) = toPathPiece x
    fromPathPiece t'
        | Just t <- T.stripSuffix ".tar.gz" t' = do
            PackageIdentifierP name version <- fromPathPiece t
            return $ PNVTarball name version
    fromPathPiece t =
        case T.breakOnEnd "-" t of
            ("", _) -> PNVName <$> fromPathPiece t
            (fromPathPiece . T.init -> Just name, fromPathPiece -> Just version) ->
                Just $ PNVNameVersion name version
            _ -> PNVName <$> fromPathPiece t


newtype HoogleVersion = HoogleVersion Text
    deriving (Show, Eq, Ord, Typeable, PathPiece)
currentHoogleVersion :: HoogleVersion
currentHoogleVersion = HoogleVersion VERSION_hoogle

data UnpackStatus = USReady
                  | USBusy
                  | USFailed !Text

data GhcMajorVersion = GhcMajorVersion !Int !Int
  deriving (Eq)

newtype GhcMajorVersionFailedParse =
    GhcMajorVersionFailedParse Text
    deriving (Show)
instance Exception GhcMajorVersionFailedParse

instance Display GhcMajorVersion where
    display (GhcMajorVersion a b) = display a <> "." <> display b

ghcMajorVersionFromText :: MonadThrow m => Text -> m GhcMajorVersion
ghcMajorVersionFromText t =
    case Reader.decimal t of
        Right (a, T.uncons -> Just ('.', t')) ->
            case Reader.decimal t' of
                Right (b, t'')
                    | T.null t'' -> return $ GhcMajorVersion a b
                _ -> failedParse
        _ -> failedParse
  where
    failedParse = throwM $ GhcMajorVersionFailedParse t

instance PersistFieldSql GhcMajorVersion where
    sqlType = sqlType . fmap textDisplay

instance PersistField GhcMajorVersion where
    toPersistValue = toPersistValue . textDisplay
    fromPersistValue v = do
        t <- fromPersistValueText v
        case ghcMajorVersionFromText t of
            Just ver -> return ver
            Nothing  -> Left $ "Cannot convert to GhcMajorVersion: " <> t

instance Hashable GhcMajorVersion where
    hashWithSalt = hashUsing textDisplay

instance FromJSON GhcMajorVersion where
    parseJSON = withText "GhcMajorVersion" $ either (fail . show) return . ghcMajorVersionFromText

instance ToJSON GhcMajorVersion where
    toJSON = toJSON . textDisplay


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
    toPathPiece Win32   = "win32"
    toPathPiece Win64   = "win64"
    toPathPiece Linux32 = "linux32"
    toPathPiece Linux64 = "linux64"
    toPathPiece Mac32   = "mac32"
    toPathPiece Mac64   = "mac64"

    fromPathPiece "win32"   = Just Win32
    fromPathPiece "win64"   = Just Win64
    fromPathPiece "linux32" = Just Linux32
    fromPathPiece "linux64" = Just Linux64
    fromPathPiece "mac32"   = Just Mac32
    fromPathPiece "mac64"   = Just Mac64
    fromPathPiece _         = Nothing


newtype CompilerP =
    CompilerGHC { ghcVersion :: VersionP }
    deriving (Eq, Ord)

instance Show CompilerP where
    show = T.unpack . textDisplay

instance FromJSONKey CompilerP where
    fromJSONKey = FromJSONKeyTextParser (either fail pure . parseCompilerP)

instance Display CompilerP where
    display (CompilerGHC vghc) = "ghc-" <> display vghc
instance ToJSON CompilerP where
    toJSON = String . textDisplay
instance FromJSON CompilerP where
    parseJSON = withText "CompilerP" (either fail return .  parseCompilerP)
instance PersistField CompilerP where
    toPersistValue = PersistText . textDisplay
    fromPersistValue v = fromPersistValue v >>= mapLeft T.pack . parseCompilerP
instance PersistFieldSql CompilerP where
    sqlType _ = SqlString

parseCompilerP :: Text -> Either String CompilerP
parseCompilerP txt =
    case T.stripPrefix "ghc-" txt of
        Just vTxt ->
            bimap displayException (CompilerGHC . VersionP) $ parseVersionThrowing (T.unpack vTxt)
        Nothing -> Left $ "Invalid prefix for compiler: " <> T.unpack txt


type Year = Int
newtype Month =
    Month Int
    deriving (Eq, Read, Show, Ord)
instance PathPiece Month where
    toPathPiece (Month i)
        | i < 10 = T.pack $ '0' : show i
        | otherwise = tshow i
    fromPathPiece "10" = Just $ Month 10
    fromPathPiece "11" = Just $ Month 11
    fromPathPiece "12" = Just $ Month 12
    fromPathPiece (T.unpack -> ['0', c])
        | '1' <= c && c <= '9' = Just $ Month $ ord c - ord '0'
    fromPathPiece _ = Nothing

newtype VersionRangeP = VersionRangeP
    { unVersionRangeP :: VersionRange
    } deriving (Eq, Show, Read, Data, NFData)
instance Display VersionRangeP where
    display = dtDisplay . unVersionRangeP
    textDisplay = dtDisplay . unVersionRangeP
instance ToMarkup VersionRangeP where
    toMarkup = dtDisplay . unVersionRangeP
instance PersistField VersionRangeP where
    toPersistValue = PersistText . textDisplay
    fromPersistValue v =
        fromPersistValue v >>= bimap (T.pack . displayException) VersionRangeP . dtParse . hackwardCompat_3_4
      where
        -- We use parseSimple under the hood, which always parses using
        -- the latest version of the Cabal spec. In practice, this hasn't
        -- been a problem. Until now.
        --
        -- Cabal spec 3.4 dropped support for "-any" as a version range, and the
        -- database is full of such values. Luckily, ">=0" is a
        -- backward-compatible synonym for "-any". New versions of this app will
        -- write ">=0" instead of "-any", which old versions of this app will
        -- understand just fine. We just need to substitute on read.
        --
        -- FIXME: strictly speaking, VersionRange cannot be parsed without
        -- knowing the Cabal spec version of the package that used it. There's
        -- nothing *wrong* with "-any". That means we probably need to decode it
        -- no further than Text and do further processing outside of the
        -- PersistField instance.
        hackwardCompat_3_4 "-any" = ">=0"
        hackwardCompat_3_4 t      = t
instance PersistFieldSql VersionRangeP where
    sqlType _ = SqlString


-- | Construct a module name from valid components
moduleNameFromComponents :: [Text] -> ModuleNameP
moduleNameFromComponents = ModuleNameP . DT.fromComponents . map T.unpack

instance ToMarkup ModuleNameP where
    toMarkup = dtDisplay . unModuleNameP
-- In urls modules are represented with dashes, instead of dots, i.e. Foo-Bar-Baz vs Foo.Bar.Baz
instance PathPiece ModuleNameP where
    toPathPiece (ModuleNameP moduleName) = T.intercalate "-" $ map T.pack $ DT.components moduleName
    fromPathPiece moduleNameDashes = do
        (moduleNameDashesNoDot, "") <- Just $ T.break (== '.') moduleNameDashes
        -- \ make sure there are no dots in the module components
        let moduleComponents = T.unpack <$> T.split (== '-') moduleNameDashesNoDot
        guard (all DT.validModuleComponent moduleComponents)
        pure $ ModuleNameP $ DT.fromComponents moduleComponents

parseModuleNameP :: String -> Maybe ModuleNameP
parseModuleNameP = fmap ModuleNameP . DT.simpleParse

newtype FlagNameP = FlagNameP
    { unFlagNameP :: FlagName
    } deriving (Eq, Ord, Show, Read, Data, NFData)

instance Display FlagNameP where
    display = dtDisplay . unFlagNameP
    textDisplay = dtDisplay . unFlagNameP

instance ToMarkup FlagNameP where
    toMarkup = dtDisplay . unFlagNameP

instance PersistField FlagNameP where
    toPersistValue = PersistText . textDisplay
    fromPersistValue v = mapLeft T.pack . parseFlagNameP =<< fromPersistValue v
instance PersistFieldSql FlagNameP where
    sqlType _ = SqlString
instance PersistField (Map FlagNameP Bool) where
    toPersistValue = toPersistValue . Map.mapKeys textDisplay
    fromPersistValue v =
        fmap Map.fromList .
        traverse (\(k, f) -> (,) <$> mapLeft T.pack (parseFlagNameP k) <*> fromPersistValue f) =<<
        getPersistMap v
instance PersistFieldSql (Map FlagNameP Bool) where
    sqlType _ = SqlString

instance FromJSON FlagNameP where
    parseJSON = withText "FlagName" (either fail pure . parseFlagNameP)
instance FromJSONKey FlagNameP where
    fromJSONKey = FromJSONKeyTextParser (either fail pure . parseFlagNameP)

parseFlagNameP :: Text -> Either String FlagNameP
parseFlagNameP = bimap displayException FlagNameP . dtParse


data Origin
    = Core
    | Hackage
    | Archive
    | GitRepo
    | HgRepo
    deriving (Show, Eq)

instance PersistField Origin where
    toPersistValue =
        toPersistValue . \case
            Core    -> 0 :: Int64
            Hackage -> 1
            Archive -> 2
            GitRepo -> 3
            HgRepo  -> 4
    fromPersistValue v =
        fromPersistValue v >>= \case
            0 -> Right Core
            1 -> Right Hackage
            2 -> Right Archive
            3 -> Right GitRepo
            4 -> Right HgRepo
            n -> Left $ "Unknown origin type: " <> textDisplay (n :: Int64)

instance PersistFieldSql Origin where
    sqlType _ = SqlInt64

instance ToJSON Origin where
    toJSON = \case
      Core    -> "core"
      Hackage -> "hackage"
      Archive -> "archive"
      GitRepo -> "git"
      HgRepo  -> "mercurial"
