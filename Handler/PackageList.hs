module Handler.PackageList where

import Import
import qualified Database.Esqueleto as E

getPackageListR :: Handler Html
getPackageListR = do
    names <- fmap (map E.unValue) $ runDB $ E.selectDistinct $ E.from $ \u -> do
        E.orderBy [E.asc $ u E.^. UploadedName]
        return $ u E.^. UploadedName
    defaultLayout $ do
        setTitle "Package list"
        $(widgetFile "package-list")
