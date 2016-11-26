import Prelude
import Stackage.Database.Cron
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    stackageServerCron
