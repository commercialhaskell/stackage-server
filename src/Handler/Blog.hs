{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Blog
  ( getBlogHomeR
  , getBlogPostR
  , getBlogFeedR
  ) where

import Data.WebsiteContent
import Import
import Yesod.AtomFeed (atomLink)
import RIO.Time (getCurrentTime)

getAddPreview :: Handler (Route App -> (Route App, [(Text, Text)]))
getAddPreview = do
  mpreview <- lookupGetParam "preview"
  case mpreview of
    Just "true" -> return $ \route -> (route, [("preview", "true")])
    _           -> return $ \route -> (route, [])

getBlogHomeR :: Handler ()
getBlogHomeR = do
    cacheSeconds 3600
    posts <- getPosts
    case headMay posts of
        Nothing -> notFound
        Just post -> do
            addPreview <- getAddPreview
            redirect $ addPreview $ BlogPostR (postYear post) (postMonth post) (postSlug post)

getBlogPostR :: Year -> Month -> Text -> Handler Html
getBlogPostR year month slug = do
    cacheSeconds 3600
    posts <- getPosts
    post <- maybe notFound return $ find matches posts
    now <- getCurrentTime
    addPreview <- getAddPreview
    defaultLayout $ do
        setTitle $ toHtml $ postTitle post
        atomLink BlogFeedR "Stackage Curator blog"
        $(widgetFile "blog-post")
        toWidgetHead [shamlet|<meta name=og:description value=#{postDescription post}>|]
  where
    matches p = postYear p == year && postMonth p == month && postSlug p == slug

getBlogFeedR :: Handler TypedContent
getBlogFeedR = do
    cacheSeconds 3600
    posts <- fmap (take 10) getPosts
    latest <- maybe notFound return $ headMay posts
    newsFeed
        Feed
            { feedTitle = "Stackage Curator blog"
            , feedLinkSelf = BlogFeedR
            , feedLinkHome = HomeR
            , feedAuthor = "The Stackage Curator team"
            , feedDescription = "Messages from the Stackage Curators about the Stackage project"
            , feedLanguage = "en"
            , feedUpdated = postTime latest
            , feedLogo = Nothing
            , feedEntries = map toEntry $ toList posts
            }
  where
    toEntry post =
        FeedEntry
            { feedEntryLink = BlogPostR (postYear post) (postMonth post) (postSlug post)
            , feedEntryUpdated = postTime post
            , feedEntryTitle = postTitle post
            , feedEntryContent = postBody post
            , feedEntryEnclosure = Nothing
            , feedEntryCategories = []
            }
