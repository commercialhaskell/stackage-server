module Handler.Download
  ( getDownloadR
  , getDownloadLtsSnapshotsJsonR
  , getGhcMajorVersionR
  ) where

import Import
import Data.Slug (SnapSlug)

executableFor :: SupportedArch -> StackageExecutable
executableFor Win32 = StackageWindowsExecutable
executableFor Win64 = StackageWindowsExecutable
executableFor _ = StackageUnixExecutable

-- TODO: link to s3
executableLink :: SupportedArch -> StackageExecutable -> Route App
executableLink arch exe =
    StaticR $ StaticRoute ["setup", toPathPiece arch, toPathPiece exe] []

downloadCandidates :: [(SupportedArch, StackageExecutable)]
downloadCandidates =
    map (\arch -> (arch, executableFor arch))
        [minBound .. maxBound]

currentlySupported :: SupportedArch -> Bool
currentlySupported Linux64 = True
currentlySupported _ = False

getDownloadR :: Handler Html
getDownloadR = defaultLayout $ do
    $(widgetFile "download")

ltsMajorVersions :: Handler [Lts]
ltsMajorVersions = liftM (map entityVal) $ runDB $ do
  mapWhileIsJustM [0..] $ \x -> do
    selectFirst [LtsMajor ==. x] [Desc LtsMinor]

mapWhileIsJustM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
mapWhileIsJustM [] _f = return []
mapWhileIsJustM (x:xs) f = f x >>= \case
    Nothing -> return []
    Just y -> (y:) `liftM` mapWhileIsJustM xs f

getDownloadLtsSnapshotsJsonR :: Handler Value
getDownloadLtsSnapshotsJsonR = liftM reverse ltsMajorVersions >>= \case
    [] -> return $ object []
    majorVersions@(latest:_) -> return $ object
         $ ["lts" .= printLts latest]
        ++ map toObj majorVersions
  where
    toObj lts@(Lts major _ _) =
        pack ("lts-" ++ show major) .= printLts lts
    printLts (Lts major minor _) =
        "lts-" ++ show major ++ "." ++ show minor

-- TODO: add this to db
ltsGhcMajorVersion :: Stackage -> Text
ltsGhcMajorVersion _ = "7.8"

getGhcMajorVersionR :: SnapSlug -> Handler Text
getGhcMajorVersionR slug = do
  snapshot <- runDB $ getBy404 $ UniqueSnapshot slug
  return $ ltsGhcMajorVersion $ entityVal snapshot
