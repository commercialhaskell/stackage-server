module Handler.BuildVersion where

import Import hiding (lift)
import Language.Haskell.TH.Syntax
import System.Process (rawSystem)
import System.Exit

getBuildVersionR :: Handler Text
getBuildVersionR = return $ pack $(do
    let headFile = ".git/HEAD"
    qAddDependentFile headFile
    ehead <- qRunIO $ tryIO $ readFile $ headFile
    case decodeUtf8 <$> ehead of
        Left e -> lift $ ".git/HEAD not read: " ++ show e
        Right raw ->
            case takeWhile (/= '\n') <$> stripPrefix "ref: " raw of
                Nothing -> lift $ ".git/HEAD not in expected format: " ++ show raw
                Just fp' -> do
                    let fp = ".git" </> unpack (fp' :: Text)
                    qAddDependentFile fp
                    bs <- qRunIO $ readFile fp
                    isDirty <- qRunIO
                             $ (/= ExitSuccess)
                           <$> rawSystem "git" ["diff-files", "--quiet"]
                    lift $ unpack $ unlines
                        [ "Most recent commit: " ++ asText (decodeUtf8 bs)
                        , "Working tree is " ++ (if isDirty then "dirty" else "clean")
                        ]
    )
