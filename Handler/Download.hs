module Handler.Download
  ( getDownloadR
  , getDownloadStackageExecutableR
  ) where

import Import

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
