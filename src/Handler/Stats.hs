module Handler.Stats (getStatsR) where

import GHC.Stats
import Import

getStatsR :: Handler String
getStatsR = liftIO $ show <$> getRTSStats
