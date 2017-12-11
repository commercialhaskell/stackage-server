module Data.GhcLinks
  ( GhcLinks(..)
  , readGhcLinks
  ) where

import ClassyPrelude.Yesod
import Control.Monad.State.Strict (modify, execStateT)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Yaml as Yaml
import Filesystem (readTextFile, isFile)

import Types


newtype GhcLinks = GhcLinks
  { ghcLinksMap :: HashMap (SupportedArch, GhcMajorVersion) Text }
  -- ^ a map from (arch, ver) to yaml

supportedArches :: [SupportedArch]
supportedArches = [minBound .. maxBound]

readGhcLinks :: FilePath -> IO GhcLinks
readGhcLinks dir = do
  let ghcMajorVersionsPath = dir </> "supported-ghc-major-versions.yaml"
  Yaml.decodeFile ghcMajorVersionsPath >>= \case
    Nothing -> return $ GhcLinks HashMap.empty
    Just (ghcMajorVersions :: [GhcMajorVersion]) -> do
      let opts =
            [ (arch, ver)
            | arch <- supportedArches
            , ver <- ghcMajorVersions
            ]
      hashMap <- flip execStateT HashMap.empty
               $ forM_ opts $ \(arch, ver) -> do
        let verText = ghcMajorVersionToText ver
            fileName = "ghc-" <> verText <> "-links.yaml"
            path = dir
              </> unpack (toPathPiece arch)
              </> unpack fileName
        whenM (liftIO $ isFile (fromString path)) $ do
          text <- liftIO $ readTextFile (fromString path)
          modify (HashMap.insert (arch, ver) text)
      return $ GhcLinks hashMap
