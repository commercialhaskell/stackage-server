module Handler.PackageList where

import Import
import qualified Database.Esqueleto as E
import Yesod.Core.Types (WidgetT (WidgetT), unWidgetT)
import Data.Time (NominalDiffTime, addUTCTime)

getPackageListR :: Handler Html
getPackageListR = do
    names <- fmap (map E.unValue) $ runDB $ E.selectDistinct $ E.from $ \u -> do
        E.orderBy [E.asc $ u E.^. UploadedName]
        return $ u E.^. UploadedName
    defaultLayout $ do
        setTitle "Package list"
        cachedWidget (5 * 60) "package-list" $(widgetFile "package-list")

-- FIXME move somewhere else, maybe even yesod-core
cachedWidget :: NominalDiffTime -> Text -> Widget -> Widget
cachedWidget diff key widget = do
    ref <- widgetCache <$> getYesod
    now <- liftIO getCurrentTime
    mpair <- lookup key <$> readIORef ref
    case mpair of
        Just (expires, gw) | expires > now -> do
            $logDebug "Using cached widget"
            WidgetT $ \_ -> return ((), gw)
        _ -> do
            $logDebug "Not using cached widget"
            WidgetT $ \hd -> do
                ((), gw) <- unWidgetT widget hd
                -- FIXME render the builders in gw for more efficiency
                atomicModifyIORef' ref $ \m -> (insertMap key (addUTCTime diff now, gw) m, ())
                return ((), gw)
