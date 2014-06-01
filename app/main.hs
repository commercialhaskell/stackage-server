import Application (makeApplication)
import Prelude (IO)
import Prelude (Bool(..))
import Settings (parseExtra)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main (defaultMainLog)

main :: IO ()
main = defaultMainLog (fromArgs parseExtra) (makeApplication False)
