module Handler.Download
  ( getDownloadR
  , getDownloadLtsSnapshotsJsonR
  , getGhcMajorVersionR
  , getDownloadGhcLinksR
  ) where

import Import
import Data.Slug (SnapSlug)
import Data.GhcLinks
import Yesod.GitRepo (grContent)

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

ltsMajorVersions :: YesodDB App [Lts]
ltsMajorVersions =
        (dropOldMinors . map entityVal)
    <$> selectList [] [Desc LtsMajor, Desc LtsMinor]

dropOldMinors :: [Lts] -> [Lts]
dropOldMinors [] = []
dropOldMinors (l@(Lts x _ _):rest) =
    l : dropOldMinors (dropWhile sameMinor rest)
  where
    sameMinor (Lts y _ _) = x == y

getDownloadLtsSnapshotsJsonR :: Handler Value
getDownloadLtsSnapshotsJsonR = do
    (mlatestNightly, ltses) <- runDB $ (,)
        <$> getLatestNightly
        <*> ltsMajorVersions
    let lts = case ltses of
            [] -> []
            majorVersions@(latest:_) ->
                   ("lts" .= printLts latest)
                 : map toObj majorVersions
        nightly = case mlatestNightly of
            Nothing -> id
            Just n -> (("nightly" .= printNightly n):)
    return $ object $ nightly lts
  where
    toObj lts@(Lts major _ _) =
        pack ("lts-" ++ show major) .= printLts lts
    printLts (Lts major minor _) =
        "lts-" ++ show major ++ "." ++ show minor

    printNightly (Entity _ (Nightly day _ _)) =
        "nightly-" ++ tshow day
    getLatestNightly = selectFirst [] [Desc NightlyDay]

-- TODO: add this to db
ltsGhcMajorVersion :: Stackage -> Text
ltsGhcMajorVersion _ = "7.8"

getGhcMajorVersionR :: SnapSlug -> Handler Text
getGhcMajorVersionR slug = do
  snapshot <- runDB $ getBy404 $ UniqueSnapshot slug
  return $ ltsGhcMajorVersion $ entityVal snapshot

getDownloadGhcLinksR :: SupportedArch -> Text -> Handler TypedContent
getDownloadGhcLinksR arch fileName = do
  ver <- maybe notFound return
       $ stripPrefix "ghc-" >=> stripSuffix "-links.yaml"
       $ fileName
  ghcLinks <- getYesod >>= fmap wcGhcLinks . liftIO . grContent . websiteContent
  case lookup (arch, ver) (ghcLinksMap ghcLinks) of
    Just text -> return $ TypedContent yamlMimeType $ toContent text
    Nothing -> notFound
  where
    yamlMimeType = "text/yaml"
