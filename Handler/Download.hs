module Handler.Download
  ( getDownloadR
  , getDownloadStackageExecutableR
  , getDownloadLtsSnapshotsJsonR
  , getDownloadEnvironmentJsonR
  ) where

import Import
import Data.Slug (SnapSlug)

executableFor :: SupportedArch -> StackageExecutable
executableFor Win32 = StackageWindowsExecutable
executableFor Win64 = StackageWindowsExecutable
executableFor _ = StackageUnixExecutable

downloadCandidates :: [(SupportedArch, StackageExecutable)]
downloadCandidates =
    map (\arch -> (arch, executableFor arch))
        [minBound .. maxBound]

getDownloadR :: Handler Html
getDownloadR = defaultLayout $ do
    $(widgetFile "download")

getDownloadStackageExecutableR
  :: SupportedArch -> StackageExecutable -> Handler Html
getDownloadStackageExecutableR arch exe = do
    -- TODO: send exeutable file instead
    when (executableFor arch /= exe) notFound
    defaultLayout $ do
        $(widgetFile "downloadExe")

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

getDownloadEnvironmentJsonR :: SnapSlug -> SupportedArch -> Handler Value
getDownloadEnvironmentJsonR _slug Linux64 = do
    -- TODO: dynamic generation based on db entries
    let ghc = object
            [ "version" .= asText "7.8.4"
            , "url" .= asText "http://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-unknown-linux-deb7.tar.xz"
            , "sha1" .= asText "11aec12d4bb27f6fa59dcc8535a7a3b3be8cb787"
            ]
        cabal = object
            [ "version" .= asText "1.20.0.3"
            , "url" .= asText "http://www.haskell.org/cabal/release/cabal-install-1.20.0.3/cabal-1.20.0.3-i386-unknown-linux.tar.gz"
            , "sha1" .= asText "647ae3e561343a709b09ed70fa6bc7b1ce39e25b"
            ]
    return $ object
        [ "ghc" .= ghc
        , "cabal" .= cabal
        ]
getDownloadEnvironmentJsonR _slug _arch = notFound
