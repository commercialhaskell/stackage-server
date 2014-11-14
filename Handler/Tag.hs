module Handler.Tag where

import qualified Database.Esqueleto as E
import           Data.Slug (Slug, unSlug)
import           Import


getTagListR :: Handler Html
getTagListR = do
    tags <- fmap (map (\(E.Value v) -> v)) $ runDB $
        E.selectDistinct $ E.from $ \tag -> do
            E.orderBy [E.asc (tag E.^. TagTag)]
            return (tag E.^. TagTag)
    defaultLayout $ do
        setTitle "Stackage tags"
        $(widgetFile "tag-list")

getTagR :: Slug -> Handler Html
getTagR tagSlug = do
    packages <- fmap (map (\(E.Value v) -> v)) $ runDB $
        E.select $ E.from $ \tag -> do
            E.where_ (tag E.^. TagTag E.==. E.val tagSlug)
            E.orderBy [E.asc (tag E.^. TagPackage)]
            return (tag E.^. TagPackage)
    let tag = unSlug tagSlug
    defaultLayout $ do
          setTitle $ "Stackage tag"
          $(widgetFile "tag")
