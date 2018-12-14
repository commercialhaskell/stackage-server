{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.GhcLinks
  ( GhcLinks(..)
  , readGhcLinks
  ) where

import Control.Monad.State.Strict (execStateT, modify)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Yaml as Yaml
import RIO
import RIO.FilePath
import RIO.Text (unpack)
import System.Directory
import Web.PathPieces

import Types


newtype GhcLinks = GhcLinks
  { ghcLinksMap :: HashMap (SupportedArch, GhcMajorVersion) Text }
  -- ^ a map from (arch, ver) to yaml

supportedArches :: [SupportedArch]
supportedArches = [minBound .. maxBound]

readGhcLinks :: FilePath -> IO GhcLinks
readGhcLinks dir = do
    let ghcMajorVersionsPath = dir </> "supported-ghc-major-versions.yaml"
    Yaml.decodeFileEither ghcMajorVersionsPath >>= \case
        Left _ -> return $ GhcLinks HashMap.empty
        Right (ghcMajorVersions :: [GhcMajorVersion]) -> do
            let opts = [(arch, ver) | arch <- supportedArches, ver <- ghcMajorVersions]
            hashMap <-
                flip execStateT HashMap.empty $
                forM_ opts $ \(arch, ver) -> do
                    let verText = textDisplay ver
                        fileName = "ghc-" <> verText <> "-links.yaml"
                        path = dir </> unpack (toPathPiece arch) </> unpack fileName
                    whenM (liftIO $ doesFileExist path) $ do
                        text <- liftIO $ readFileUtf8 path
                        modify (HashMap.insert (arch, ver) text)
            return $ GhcLinks hashMap
