module Handler.PackageList where

import qualified Data.HashMap.Strict as M
import           Data.Time (NominalDiffTime, addUTCTime)
import qualified Database.Esqueleto as E
import           Import
import           Yesod.Core.Types (WidgetT (WidgetT), unWidgetT)

getPackageListR :: Handler Html
getPackageListR = do
    packages <- fmap (uniqueByKey . map (E.unValue***strip . E.unValue)) $ runDB $
        E.selectDistinct $ E.from $ \(u,m) -> do
          E.where_ (m E.^. MetadataName E.==. u E.^. UploadedName)
          E.orderBy [E.asc $ u E.^. UploadedName]
          return $ (u E.^. UploadedName
                   ,m E.^. MetadataSynopsis)
    defaultLayout $ do
        setTitle "Package list"
        $(combineStylesheets 'StaticR
            [ css_bootstrap_css
            , css_bootstrap_responsive_css
            ])
        cachedWidget (5 * 60) "package-list" $(widgetFile "package-list")
  where strip x = fromMaybe x (stripSuffix "." x)
        uniqueByKey = sortBy (comparing fst) . M.toList . M.fromList

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
