{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Data.WebsiteContent
    ( WebsiteContent (..)
    , StackRelease (..)
    , Post (..)
    , loadWebsiteContent
    ) where

import ClassyPrelude.Yesod
import CMarkGFM
import Data.Aeson (withObject)
import Data.GhcLinks
import Data.Yaml
import System.FilePath (takeFileName)
import Text.Blaze.Html (preEscapedToHtml)
import Types

data WebsiteContent = WebsiteContent
    { wcHomepage      :: !Html
    , wcAuthors       :: !Html
    , wcOlderReleases :: !Html
    , wcGhcLinks      :: !GhcLinks
    , wcStackReleases :: ![StackRelease]
    , wcPosts         :: !(Vector Post)
    , wcSpamPackages  :: !(Set PackageNameP)
    -- ^ Packages considered spam which should not be displayed.
    }

data Post = Post
  { postTitle       :: !Text
  , postSlug        :: !Text
  , postAuthor      :: !Text
  , postTime        :: !UTCTime
  , postDescription :: !Text
  , postBody        :: !Html
  }

loadWebsiteContent :: FilePath -> IO WebsiteContent
loadWebsiteContent dir = do
    wcHomepage <- readHtml "homepage.html"
    wcAuthors <- readHtml "authors.html"
    wcOlderReleases <- readHtml "older-releases.html" `catchIO`
                    \_ -> readMarkdown "older-releases.md"
    wcGhcLinks <- readGhcLinks $ dir </> "stackage-cli"
    wcStackReleases <- decodeFileEither (dir </> "stack" </> "releases.yaml")
        >>= either throwIO return
    wcPosts <- loadPosts (dir </> "posts") `catchAny` \e -> do
      putStrLn $ "Error loading posts: " ++ tshow e
      return mempty
    wcSpamPackages <- decodeFileEither (dir </> "spam-packages.yaml")
                  >>= either throwIO (return . setFromList)
    return WebsiteContent {..}
  where
    readHtml fp = fmap preEscapedToMarkup $ readFileUtf8 $ dir </> fp
    readMarkdown fp = fmap (preEscapedToHtml . commonmarkToHtml
                        [optSmart]
                        [extTable, extAutolink])
               $ readFileUtf8 $ dir </> fp

loadPosts :: FilePath -> IO (Vector Post)
loadPosts dir =
     fmap (sortBy (\x y -> postTime y `compare` postTime x))
   $ runConduitRes
   $ sourceDirectory dir
  .| concatMapC (stripSuffix ".md")
  .| mapMC loadPost
  .| sinkVector
  where
    loadPost :: FilePath -> ResourceT IO Post
    loadPost noExt = handleAny (\e -> throwString $ "Could not parse " ++ noExt ++ ".md: " ++ show e) $ do
      bs <- readFile $ noExt ++ ".md"
      let slug = pack $ takeFileName noExt
          text = filter (/= '\r') $ decodeUtf8 bs
      (frontmatter, body) <-
        case lines text of
          "---":rest ->
            case break (== "---") rest of
              (frontmatter, "---":body) -> return (unlines frontmatter, unlines body)
              _ -> error "Missing closing --- on frontmatter"
          _ -> error "Does not start with --- frontmatter"
      case Data.Yaml.decodeEither' $ encodeUtf8 frontmatter of
        Left e -> throwIO e
        Right mkPost -> return $ mkPost slug $ preEscapedToHtml $ commonmarkToHtml
          [optSmart]
          [extTable, extAutolink]
          body

instance (slug ~ Text, body ~ Html) => FromJSON (slug -> body -> Post) where
  parseJSON = withObject "Post" $ \o -> do
    postTitle <- o .: "title"
    postAuthor <- o .: "author"
    postTime <- o .: "timestamp"
    postDescription <- o .: "description"
    return $ \postSlug postBody -> Post {..}

data StackRelease = StackRelease
    { srName    :: !Text
    , srPattern :: !Text
    }
instance FromJSON StackRelease where
    parseJSON = withObject "StackRelease" $ \o -> StackRelease
        <$> o .: "name"
        <*> o .: "pattern"
