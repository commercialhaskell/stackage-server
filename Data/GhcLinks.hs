module Data.GhcLinks
  ( GhcLinks(..)
  , readGhcLinks
  ) where

import ClassyPrelude.Yesod
import Control.Monad.State.Strict (modify, execStateT)
import qualified Data.HashMap.Strict as HashMap
import Filesystem (readTextFile, isFile)

import Types


newtype GhcLinks = GhcLinks
  { ghcLinksMap :: HashMap (SupportedArch, GhcMajorVersion) Text }
  -- ^ a map from (arch, ver) to yaml

supportedArches :: [SupportedArch]
supportedArches = [minBound .. maxBound]

supportedGhcMajorVersions :: [GhcMajorVersion]
supportedGhcMajorVersions = ["7.8", "7.10"]


readGhcLinks :: FilePath -> IO GhcLinks
readGhcLinks dir = do
  let opts =
        [ (arch, ver)
        | arch <- supportedArches
        , ver <- supportedGhcMajorVersions
        ]
  hashMap <- flip execStateT HashMap.empty
           $ forM_ opts $ \(arch, ver) -> do
    let fileName = "ghc-" <> ver <> "-links.yaml"
    let path = dir
          </> fpFromText (toPathPiece arch)
          </> fpFromText fileName
    whenM (liftIO $ isFile path) $ do
      text <- liftIO $ readTextFile path
      modify (HashMap.insert (arch, ver) text)
  return $ GhcLinks hashMap
