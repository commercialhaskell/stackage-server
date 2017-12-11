module Handler.System where

import Import
import System.Process (readProcess)

getSystemR :: Handler String
getSystemR = track "Handler.System.getSystemR" $
    liftIO $ readProcess "df" ["-ih"] ""
