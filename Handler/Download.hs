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

getDownloadR :: Handler Html
getDownloadR = track "Hoogle.Download.getDownloadR" $
    redirectWith status301 InstallR

getDownloadSnapshotsJsonR :: Handler Value
getDownloadSnapshotsJsonR = track "Hoogle.Download.getDownloadSnapshotsJsonR"
    getDownloadLtsSnapshotsJsonR

getDownloadLtsSnapshotsJsonR :: Handler Value
getDownloadLtsSnapshotsJsonR = track "Hoogle.Download.getDownloadLtsSnapshotsJsonR"
    snapshotsJSON

-- Print the ghc major version for the given snapshot.
ghcMajorVersionText :: Snapshot -> Text
ghcMajorVersionText =
    getMajorVersion . snapshotGhc
  where
    getMajorVersion :: Text -> Text
    getMajorVersion = intercalate "." . take 2 . T.splitOn "."

getGhcMajorVersionR :: SnapName -> Handler Text
getGhcMajorVersionR name = track "Hoogle.Download.getGhcMajorVersionR" $ do
    snapshot <- lookupSnapshot name >>= maybe notFound return
    return $ ghcMajorVersionText $ entityVal snapshot

getDownloadGhcLinksR :: SupportedArch -> Text -> Handler TypedContent
getDownloadGhcLinksR arch fileName = track "Hoogle.Download.getDownloadGhcLinksR" $ do
  ver <- maybe notFound return
       $ stripPrefix "ghc-"
         >=> stripSuffix "-links.yaml"
         >=> ghcMajorVersionFromText
       $ fileName
  ghcLinks <- getYesod >>= fmap wcGhcLinks . liftIO . grContent . appWebsiteContent
  case lookup (arch, ver) (ghcLinksMap ghcLinks) of
    Just text -> return $ TypedContent yamlMimeType $ toContent text
    Nothing -> notFound
  where
    yamlMimeType = "text/yaml"
