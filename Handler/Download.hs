module Handler.Download
  ( getDownloadR
  , getDownloadSnapshotsJsonR
  , getDownloadLtsSnapshotsJsonR
  , getGhcMajorVersionR
  , getDownloadGhcLinksR
  ) where

import Import
import Data.GhcLinks
import Yesod.GitRepo (grContent)
import Stackage.Database
import qualified Data.Text as T

executableFor :: SupportedArch -> StackageExecutable
executableFor Win32 = StackageWindowsExecutable
executableFor Win64 = StackageWindowsExecutable
executableFor _ = StackageUnixExecutable

-- TODO: link to s3
executableLink :: SupportedArch -> StackageExecutable -> Text
executableLink arch exe =
    "https://s3.amazonaws.com/download.fpcomplete.com/stackage-cli/"
    <> toPathPiece arch <> "/" <> toPathPiece exe

downloadCandidates :: [(SupportedArch, StackageExecutable)]
downloadCandidates =
    map (\arch -> (arch, executableFor arch))
        [minBound .. maxBound]

currentlySupported :: SupportedArch -> Bool
currentlySupported Linux64 = True
currentlySupported _ = False

getDownloadR :: Handler Html
getDownloadR = defaultLayout $ do
    setTitle "Download"
    $(widgetFile "download")

getDownloadSnapshotsJsonR :: Handler Value
getDownloadSnapshotsJsonR = getDownloadLtsSnapshotsJsonR

getDownloadLtsSnapshotsJsonR :: Handler Value
getDownloadLtsSnapshotsJsonR = snapshotsJSON

-- Print the ghc major version for the given snapshot.
ghcMajorVersionText :: Snapshot -> Text
ghcMajorVersionText =
    getMajorVersion . snapshotGhc
  where
    getMajorVersion :: Text -> Text
    getMajorVersion = intercalate "." . take 2 . T.splitOn "."

getGhcMajorVersionR :: SnapName -> Handler Text
getGhcMajorVersionR name = do
    snapshot <- lookupSnapshot name >>= maybe notFound return
    return $ ghcMajorVersionText $ entityVal snapshot

getDownloadGhcLinksR :: SupportedArch -> Text -> Handler TypedContent
getDownloadGhcLinksR arch fileName = do
  ver <- maybe notFound return
       $ stripPrefix "ghc-"
         >=> stripSuffix "-links.yaml"
         >=> ghcMajorVersionFromText
       $ fileName
  ghcLinks <- getYesod >>= fmap wcGhcLinks . liftIO . grContent . websiteContent
  case lookup (arch, ver) (ghcLinksMap ghcLinks) of
    Just text -> return $ TypedContent yamlMimeType $ toContent text
    Nothing -> notFound
  where
    yamlMimeType = "text/yaml"
