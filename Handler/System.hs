module Handler.System where

import Import
import System.Process (readProcess)

getSystemR :: Handler String
getSystemR = liftIO $ do
    x <- readProcess "df" ["-ih"] ""
    y <- readProcess "ls" ["-lh", "/tmp"] ""
    return $ unlines [x, y]
