module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X
import Foundation            as X
import Test.Hspec            as X
import Yesod.Default.Config2 (ignoreEnv, loadYamlSettings)
import Yesod.Test            as X

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)
