import Application (makeApplication)
import Prelude (Bool(..), IO, elem, putStrLn)
import Settings (parseExtra)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main (defaultMainLog)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if "--summary" `elem` args
        then putStrLn "Run the server software for www.stackage.org"
        else defaultMainLog (fromArgs parseExtra) (makeApplication False)
