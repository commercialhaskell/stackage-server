module Handler.System where

import Import
import System.Process (readProcess)

getSystemR :: Handler String
getSystemR = liftIO $ readProcess "df" ["-ih"] ""
