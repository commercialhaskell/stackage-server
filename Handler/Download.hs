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

{- FIXME
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
-}

getDownloadSnapshotsJsonR :: Handler Value
getDownloadSnapshotsJsonR = getDownloadLtsSnapshotsJsonR

getDownloadLtsSnapshotsJsonR :: Handler Value
getDownloadLtsSnapshotsJsonR = do
    error "getDownloadLtsSnapshotsJsonR"
    {-
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

-- Print the ghc major version for the given snapshot.
-- Assumes 7.8 if unspecified
ghcMajorVersionText :: Stackage -> Text
ghcMajorVersionText snapshot
  = ghcMajorVersionToText
  $ fromMaybe (GhcMajorVersion 7 8)
  $ stackageGhcMajorVersion snapshot
    -}

getGhcMajorVersionR :: SnapName -> Handler Text
getGhcMajorVersionR _slug = do
  error "getGhcMajorVersionR"
  {-
  snapshot <- runDB $ getBy404 $ UniqueSnapshot slug
  return $ ghcMajorVersionText $ entityVal snapshot
  -}

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
