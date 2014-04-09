import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMainLog)
import Settings             (parseExtra)
import Application          (makeApplication)

main :: IO ()
main = defaultMainLog (fromArgs parseExtra) makeApplication
