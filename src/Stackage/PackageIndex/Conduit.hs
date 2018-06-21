{-# LANGUAGE RankNTypes #-}
module Stackage.PackageIndex.Conduit
    ( sourceTarFile
    , sourceAllCabalFiles
    , parseDistText
    , renderDistText
    , CabalFileEntry (..)
    ) where

import qualified Codec.Archive.Tar                     as Tar
import           Codec.Compression.GZip                (decompress)
import           Control.Monad                         (guard)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans.Resource          (MonadResource)
import qualified Data.ByteString.Lazy                  as L
import           Data.Conduit                          (ConduitT, bracketP, yield, (.|))
import qualified Data.Conduit.List                     as CL
import           Data.Version                          (Version)
import           Distribution.Compat.ReadP             (readP_to_S)
import           Distribution.Package                  (PackageName)
import           Distribution.PackageDescription       (GenericPackageDescription)
import           Distribution.PackageDescription.Parsec (ParseResult, parseGenericPackageDescription)
import           Distribution.Text                     (disp, parse)
import qualified Distribution.Text
import           System.IO                             (openBinaryFile)
import           Text.PrettyPrint                      (render)
import Prelude
import UnliftIO

sourceTarFile :: MonadResource m
              => Bool -- ^ ungzip?
              -> FilePath
              -> ConduitT i Tar.Entry m ()
sourceTarFile toUngzip fp = do
    bracketP (openBinaryFile fp ReadMode) hClose $ \h -> do
        lbs <- liftIO $ L.hGetContents h
        loop $ Tar.read $ ungzip' lbs
  where
    ungzip'
        | toUngzip = decompress
        | otherwise = id
    loop Tar.Done = return ()
    loop (Tar.Fail e) = throwIO e
    loop (Tar.Next e es) = yield e >> loop es

data CabalFileEntry = CabalFileEntry
    { cfeName    :: !PackageName
    , cfeVersion :: !Version
    , cfeRaw     :: L.ByteString
    , cfeEntry   :: Tar.Entry
    , cfeParsed  :: ParseResult GenericPackageDescription
    }

sourceAllCabalFiles
    :: MonadResource m
    => IO FilePath
    -> ConduitT i CabalFileEntry m ()
sourceAllCabalFiles getIndexTar = do
    tarball <- liftIO $ getIndexTar
    sourceTarFile False tarball .| CL.mapMaybe go
  where
    go e =
        case (toPkgVer $ Tar.entryPath e, Tar.entryContent e) of
            (Just (name, version), Tar.NormalFile lbs _) -> Just CabalFileEntry
                { cfeName = name
                , cfeVersion = version
                , cfeRaw = lbs
                , cfeEntry = e
                , cfeParsed = parseGenericPackageDescription $ L.toStrict lbs
                }
            _ -> Nothing

    toPkgVer s0 = do
        (name', '/':s1) <- Just $ break (== '/') s0
        (version', '/':s2) <- Just $ break (== '/') s1
        guard $ s2 == (name' ++ ".cabal")
        name <- parseDistText name'
        version <- parseDistText version'
        Just (name, version)

parseDistText :: (Monad m, Distribution.Text.Text t) => String -> m t
parseDistText s =
    case map fst $ filter (null . snd) $ readP_to_S parse s of
        [x] -> return x
        _ -> fail $ "Could not parse: " ++ s

renderDistText :: Distribution.Text.Text t => t -> String
renderDistText = render . disp
