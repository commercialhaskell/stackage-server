{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module Stackage.Database.PackageInfo
    ( PackageInfo(..)
    , Identifier(..)
    , renderEmail
    , toPackageInfo
    , parseCabalBlob
    , parseCabalBlobMaybe
    , extractDependencies
    , extractModuleNames
    , getSynopsis
    , isMarkdownFilePath
    ) where

import CMarkGFM
import Data.Char (isSpace)
import Data.Coerce
import Data.Map.Merge.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.Package (Dependency(..))
import Distribution.PackageDescription (CondTree(..), Condition(..),
                                        ConfVar(..),
                                        Flag(flagDefault, flagName), FlagName,
                                        GenericPackageDescription, author,
                                        condExecutables, condLibrary,
                                        description, genPackageFlags, homepage,
                                        license, maintainer, packageDescription,
                                        synopsis)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription,
                                               runParseResult)
import Distribution.Pretty (prettyShow)
import Distribution.System (Arch(X86_64), OS(Linux))
import Distribution.Types.CondTree (CondBranch(..))
import Distribution.Types.Library (exposedModules)
import Distribution.Types.PackageDescription (PackageDescription(package))
import Distribution.Types.VersionRange (VersionRange, intersectVersionRanges,
                                        normaliseVersionRange, withinRange)
import Distribution.Utils.ShortText (fromShortText)
import Distribution.Version (simplifyVersionRange)
import RIO
import qualified RIO.Map as Map
import qualified RIO.Map.Unchecked as Map (mapKeysMonotonic)
import Stackage.Database.Haddock (renderHaddock)
import Stackage.Database.Types (Changelog(..), Readme(..))
import Text.Blaze.Html (Html, preEscapedToHtml, toHtml)
import Text.Email.Validate
import Types (CompilerP(..), FlagNameP(..), ModuleNameP(..), PackageIdentifierP,
              PackageNameP(..), SafeFilePath, VersionP(..), VersionRangeP(..),
              unSafeFilePath, dtDisplay)
import Yesod.Form.Fields (Textarea(..))


data PackageInfo = PackageInfo
    { piSynopsis    :: !Text
    , piDescription :: !Html
    , piAuthors     :: ![Identifier]
    , piMaintainers :: ![Identifier]
    , piHomepage    :: !(Maybe Text)
    , piLicenseName :: !Text
    , piReadme      :: !Html
    , piChangelog   :: !Html
    }


toPackageInfo ::
       GenericPackageDescription
    -> Maybe Readme
    -> Maybe Changelog
    -> PackageInfo
toPackageInfo gpd mreadme mchangelog =
    PackageInfo
        { piSynopsis = T.pack $ fromShortText $ synopsis pd
        , piDescription = renderHaddock $ fromShortText (description pd)
        , piReadme = maybe mempty (\(Readme bs isMarkdown) -> renderContent bs isMarkdown) mreadme
        , piChangelog =
              maybe mempty (\(Changelog bs isMarkdown) -> renderContent bs isMarkdown) mchangelog
        , piAuthors = parseIdentitiesLiberally $ T.pack . fromShortText $ author pd
        , piMaintainers = parseIdentitiesLiberally $ T.pack . fromShortText $ maintainer pd
        , piHomepage =
              case T.strip . T.pack . fromShortText $ homepage pd of
                  "" -> Nothing
                  x  -> Just x
        , piLicenseName = T.pack $ prettyShow $ license pd
        }
  where
    pd = packageDescription gpd
    renderContent bs isMarkdown =
        let txt = decodeUtf8With lenientDecode bs
         in if isMarkdown
                then preEscapedToHtml $ commonmarkToHtml [optSmart] [extTable, extAutolink] txt
                else toHtml $ Textarea txt

getSynopsis :: GenericPackageDescription -> Text
getSynopsis = T.pack . fromShortText . synopsis . packageDescription

extractModuleNames :: GenericPackageDescription -> [ModuleNameP]
extractModuleNames = maybe [] (coerce . exposedModules . condTreeData) . condLibrary


isMarkdownFilePath :: SafeFilePath -> Bool
isMarkdownFilePath sfp =
    case T.split (== '.') $ unSafeFilePath sfp of
        [_, "md"]       -> True
        [_, "markdown"] -> True
        _               -> False


extractDependencies ::
       CompilerP -> Map FlagNameP Bool -> GenericPackageDescription -> Map PackageNameP VersionRangeP
extractDependencies compiler flags gpd =
    fmap VersionRangeP $
    combineDeps $
    maybeToList (getDeps' <$> condLibrary gpd) ++ map (getDeps' . snd) (condExecutables gpd)
  where
    getDeps' :: CondTree ConfVar [Dependency] a -> Map PackageNameP VersionRange
    getDeps' = getDeps (getCheckCond compiler (Map.mapKeysMonotonic unFlagNameP flags) gpd)

-- | Parse a cabal blob and throw an error on failure.
parseCabalBlob :: ByteString -> GenericPackageDescription
parseCabalBlob cabalBlob =
    case snd $ runParseResult $ parseGenericPackageDescription cabalBlob of
        Left err  -> error $ "Problem parsing cabal blob: " <> show err
        Right gpd -> gpd


parseCabalBlobMaybe ::
       (MonadIO m, MonadReader env m, HasLogFunc env)
    => PackageIdentifierP
    -> ByteString
    -> m (Maybe GenericPackageDescription)
parseCabalBlobMaybe pidp cabalBlob =
    case snd $ runParseResult $ parseGenericPackageDescription cabalBlob of
        Left err ->
            Nothing <$
            logError
                ("Problem parsing cabal blob for '" <> display pidp <> "': " <> displayShow err)
        Right gpd -> do
            let pid = package (packageDescription gpd)
            unless (textDisplay (dtDisplay pid :: Utf8Builder) == textDisplay pidp) $
                logError $
                "Supplied package identifier: '" <> display pidp <>
                "' does not match the one in cabal file: '" <>
                dtDisplay pid
            pure $ Just gpd

getCheckCond ::
       CompilerP -> Map FlagName Bool -> GenericPackageDescription -> Condition ConfVar -> Bool
getCheckCond compiler overrideFlags gpd = go
  where
    go (Var (OS os)) = os == Linux -- arbitrary
    go (Var (Arch arch)) = arch == X86_64 -- arbitrary
    go (Var (Flag flag)) = fromMaybe False $ Map.lookup flag flags
    go (Var (Impl flavor range)) = flavor == compilerFlavor && compilerVersion `withinRange` range
    go (Lit b) = b
    go (CNot c) = not $ go c
    go (CAnd x y) = go x && go y
    go (COr x y) = go x || go y
    (compilerFlavor, compilerVersion) =
        case compiler of
            CompilerGHC ver -> (GHC, unVersionP ver)
    flags =
        Map.merge
            Map.dropMissing -- unknown flags should be discarded
            Map.preserveMissing -- non-overriden flags stay as default
            (Map.zipWithMatched (\_flagName new _default -> new)) -- override the flag
            overrideFlags $
        Map.fromList $ map toPair $ genPackageFlags gpd
      where
        toPair f = (flagName f, flagDefault f)

getDeps ::
       (Condition ConfVar -> Bool)
    -> CondTree ConfVar [Dependency] a
    -> Map PackageNameP VersionRange
getDeps checkCond = goTree
  where
    goTree (CondNode _data deps comps) =
        combineDeps $
        map (\(Dependency name range _) -> Map.singleton (PackageNameP name) range) deps ++
        map goComp comps
    goComp (CondBranch cond yes no)
        | checkCond cond = goTree yes
        | otherwise = maybe Map.empty goTree no


combineDeps :: [Map PackageNameP VersionRange] -> Map PackageNameP VersionRange
combineDeps =
    Map.unionsWith
        (\x -> normaliseVersionRange . simplifyVersionRange . intersectVersionRanges x)



-- | An identifier specified in a package. Because this field has
-- quite liberal requirements, we often encounter various forms. A
-- name, a name and email, just an email, or maybe nothing at all.
data Identifier
  = EmailOnly !EmailAddress -- ^ An email only e.g. jones@example.com
  | Contact !Text
            !EmailAddress -- ^ A contact syntax, e.g. Dave Jones <jones@example.com>
  | PlainText !Text -- ^ Couldn't parse anything sensible, leaving as-is.
  deriving (Show,Eq)

-- | An author/maintainer field may contain a comma-separated list of
-- identifiers. It may be the case that a person's name is written as
-- "Einstein, Albert", but we only parse commas when there's an
-- accompanying email, so that would be:
--
--  Einstein, Albert <emc2@gmail.com>, Isaac Newton <falling@apple.com>
--
-- Whereas
--
-- Einstein, Albert, Isaac Newton
--
-- Will just be left alone. It's an imprecise parsing because the
-- input is wide open, but it's better than nothing:
--
-- λ> parseIdentitiesLiberally "Chris Done, Dave Jones <chrisdone@gmail.com>, Einstein, Albert, Isaac Newton, Michael Snoyman <michael@snoyman.com>"
-- [PlainText "Chris Done"
-- ,Contact "Dave Jones" "chrisdone@gmail.com"
-- ,PlainText "Einstein, Albert, Isaac Newton"
-- ,Contact "Michael Snoyman" "michael@snoyman.com"]
--
-- I think that is quite a predictable and reasonable result.
--
parseIdentitiesLiberally :: Text -> [Identifier]
parseIdentitiesLiberally =
    filter (not . emptyPlainText) .
    map strip .
    concatPlains .
    map parseChunk .
    T.split (== ',')
    where emptyPlainText (PlainText e) = T.null e
          emptyPlainText _             = False
          strip (PlainText t) = PlainText (T.strip t)
          strip x             = x
          concatPlains = go
            where go (PlainText x:PlainText y:xs) =
                    go (PlainText (x <> "," <> y) :
                        xs)
                  go (x:xs) = x : go xs
                  go [] = []

-- | Try to parse a chunk into an identifier.
--
-- 1. First tries to parse an \"email@domain.com\".
-- 2. Then tries to parse a \"Foo <email@domain.com>\".
-- 3. Finally gives up and returns a plain text.
--
-- λ> parseChunk "foo@example.com"
-- EmailOnly "foo@example.com"
-- λ> parseChunk "Dave Jones <dave@jones.com>"
-- Contact "Dave Jones" "dave@jones.com"
-- λ> parseChunk "<x>"
-- PlainText "<x>"
-- λ> parseChunk "Hello!"
-- PlainText "Hello!"
--
parseChunk :: Text -> Identifier
parseChunk chunk =
    case emailAddress (T.encodeUtf8 (T.strip chunk)) of
      Just email -> EmailOnly email
      Nothing ->
        case T.stripPrefix
               ">"
               (T.dropWhile isSpace
                            (T.reverse chunk)) of
          Just rest ->
            case T.span (/= '<') rest of
              (T.reverse -> emailStr,this) ->
                case T.stripPrefix "< " this of
                  Just (T.reverse -> name) ->
                    case emailAddress (T.encodeUtf8 (T.strip emailStr)) of
                      Just email ->
                        Contact (T.strip name) email
                      _ -> plain
                  _ -> plain
          _ -> plain
    where plain = PlainText chunk

-- | Render email to text.
renderEmail :: EmailAddress -> Text
renderEmail = T.decodeUtf8 . toByteString
