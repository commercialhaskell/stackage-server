module Handler.PackageCounts
    ( getPackageCountsR
    ) where

import Import hiding (Value (..), groupBy, (==.))
import Data.Slug (mkSlug)
import Database.Esqueleto

data Count = Count
    { name :: Text
    , date :: Day
    , packages :: Int
    }

toCount :: (Value Text, Value UTCTime, Value Int) -> Count
toCount (Value x, Value y, Value z) =
    Count x (utctDay y) z

getPackageCountsR :: Handler Html
getPackageCountsR = do
    admins <- adminUsers <$> getExtra
    counts <- runDB $ do
        let slugs = mapMaybe mkSlug $ setToList admins
        adminUids <- selectKeysList [UserHandle <-. slugs] []
        fmap (map toCount) $ select $ from $ \(s, p) -> do
            where_ $
                (not_ $ s ^. StackageTitle `like` val "%inclusive") &&.
                (s ^. StackageId ==. p ^. PackageStackage) &&.
                (s ^. StackageUser `in_` valList adminUids)
            groupBy (s ^. StackageTitle, s ^. StackageUploaded)
            orderBy [desc $ s ^. StackageUploaded]
            return
                ( s ^. StackageTitle
                , s ^. StackageUploaded
                , countRows
                )
    defaultLayout $ do
        setTitle "Package counts"
        $(widgetFile "package-counts")
