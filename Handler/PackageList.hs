module Handler.PackageList where

import qualified Data.HashMap.Strict as M
import           Data.Time (NominalDiffTime)
import qualified Database.Esqueleto as E
import           Import


-- FIXME maybe just redirect to the LTS or nightly package list
getPackageListR :: Handler Html
getPackageListR = defaultLayout $ do
    setTitle "Package list"
    cachedWidget (20 * 60) "package-list" $ do
        let clean (x, y) =
                ( E.unValue x
                , strip $ E.unValue y
                )
            addDocs (x, y) = (x, Nothing, y, Nothing)
        packages <- fmap (map addDocs . uniqueByKey . map clean) $ handlerToWidget $ runDB $
            E.selectDistinct $ E.from $ \m -> do
              E.orderBy [E.asc $ m E.^. MetadataName]
              return $ (m E.^. MetadataName
                       ,m E.^. MetadataSynopsis)
        $(widgetFile "package-list")
  where strip x = fromMaybe x (stripSuffix "." x)
        uniqueByKey = sortBy (comparing fst) . M.toList . M.fromList
        mback = Nothing

-- FIXME move somewhere else, maybe even yesod-core
cachedWidget :: NominalDiffTime -> Text -> Widget -> Widget
cachedWidget _diff _key widget = do
    -- Temporarily disabled, seems to be eating up too much memory
    widget
    {-
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
                -}
