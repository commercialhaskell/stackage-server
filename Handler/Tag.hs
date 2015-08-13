module Handler.Tag where

import qualified Database.Esqueleto as E
import           Data.Slug (Slug, unSlug)
import           Import
import Stackage.Database

getTagListR :: Handler Html
getTagListR = do
    tags <- fmap (zip [0::Int ..] . (map (\(E.Value v,E.Value i) -> (v,i::Int)))) $ runDB $
        E.select $ E.from $ \(tag `E.LeftOuterJoin` bt) -> do
            E.groupBy (tag E.^. TagTag)
            E.orderBy [E.desc (E.count (tag E.^. TagTag) :: E.SqlExpr (E.Value Int))]
            E.on $ tag E.^. TagTag E.==. bt E.^. BannedTagTag
            E.where_ $ E.isNothing $ E.just $ bt E.^. BannedTagTag
            return (tag E.^. TagTag, E.count (tag E.^. TagTag))
    defaultLayout $ do
        setTitle "Stackage tags"
        $(widgetFile "tag-list")

getTagR :: Slug -> Handler Html
getTagR tagSlug = do
    -- FIXME arguably: check if this tag is banned. Leaving it as displayed for
    -- now, since someone needs to go out of their way to find it.
    packages' <- runDB $ E.select $ E.from $ \tag -> do
        E.groupBy (tag E.^. TagPackage)
        E.where_ $ tag E.^. TagTag E.==. E.val tagSlug
        E.orderBy [E.asc $ tag E.^. TagPackage]
        return $ tag E.^. TagPackage
    packages <- fmap catMaybes $ forM packages' $ \(E.Value pname) -> do
        mp <- getPackage $ toPathPiece pname
        return $ case mp of
            Nothing -> Nothing
            Just (Entity _ p) -> Just (pname, strip $ packageSynopsis p)
    let tag = unSlug tagSlug
    defaultLayout $ do
          setTitle $ "Stackage tag"
          $(widgetFile "tag")
  where strip x = fromMaybe x (stripSuffix "." x)
