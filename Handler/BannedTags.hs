module Handler.BannedTags where

import Import
import Data.Slug (unSlug, mkSlug, Slug)

checkSlugs :: Monad m => Textarea -> m (Either Text [Slug])
checkSlugs (Textarea t) =
    return $ first tshow $ (mapM mkSlug $ filter (not . null) $ lines $ filter (/= '\r') t)

fromSlugs :: [Slug] -> Textarea
fromSlugs = Textarea . unlines . map unSlug

getBannedTagsR :: Handler Html
getBannedTagsR = do
    Entity _ user <- requireAuth
    extra <- getExtra
    when (unSlug (userHandle user) `notMember` adminUsers extra)
        $ permissionDenied "You are not an administrator"
    curr <- fmap (map (bannedTagTag . entityVal))
          $ runDB $ selectList [] [Asc BannedTagTag]
    ((res, widget), enctype) <- runFormPost $ renderDivs
        $ fmap (fromMaybe [])
        $ aopt
            (checkMMap checkSlugs fromSlugs textareaField)
            "Banned tags (one per line)" $ Just (Just curr)
    case res of
        FormSuccess tags -> do
            runDB $ do
                deleteWhere ([] :: [Filter BannedTag])
                insertMany_ $ map BannedTag tags
            setMessage "Tags updated"
            redirect BannedTagsR
        _ -> defaultLayout $ do
            setTitle "Banned Tags"
            $(widgetFile "banned-tags")

putBannedTagsR :: Handler Html
putBannedTagsR = getBannedTagsR
