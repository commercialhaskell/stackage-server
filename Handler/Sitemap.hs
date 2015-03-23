module Handler.Sitemap (getSitemapR) where

import Import
import Yesod.Sitemap
import Data.List (nub)

type Sitemap = Source Handler (SitemapUrl (Route App))


getSitemapR :: Handler TypedContent
getSitemapR = sitemap $ do
    priority 1.0 $ HomeR

    priority 0.9 $ LtsR []
    priority 0.8 $ NightlyR []

    priority 0.7 $ AllSnapshotsR
    priority 0.7 $ PackageListR

    priority 0.6 $ TagListR
    priority 0.6 $ AuthorsR
    priority 0.6 $ InstallR
    priority 0.6 $ OlderReleasesR

    url PackageCountsR

    selectAll >>= ltsSitemaps
    selectAll >>= mapM_ snapshotSitemap
    selectAll >>= mapM_ packageMetadataSitemap
    selectAll >>= mapM_ tagSitemap


selectAll :: (PersistEntity val, PersistEntityBackend val ~ YesodPersistBackend App)
  => ConduitM () (SitemapUrl (Route App)) Handler [val]
selectAll = lift $ runDB $ fmap (map entityVal) $ selectList [] []

ltsSitemaps :: [Lts] -> Sitemap
ltsSitemaps ltss = do
  ltsMajorSitemap ltss
  mapM_ ltsSitemap ltss

ltsMajorSitemap :: [Lts] -> Sitemap
ltsMajorSitemap ltss = mapM_ go majorVersions
  where
    majorVersions = nub $ map ltsMajor ltss
    go ver = priority 0.55 $ LtsR [pack (show ver)]

ltsSitemap :: Lts -> Sitemap
ltsSitemap lts = url $ LtsR [slug]
  where
    slug = show' (ltsMajor lts) <> "." <> show' (ltsMinor lts)
    show' = pack . show

snapshotSitemap :: Stackage -> Sitemap
snapshotSitemap s = do
    url' StackageHomeR
    url' StackageMetadataR
    url' StackageCabalConfigR
    url' StackageIndexR
    url' SnapshotPackagesR
    url' DocsR
    url' HoogleR
  where
    url' = url . SnapshotR (stackageSlug s)

packageMetadataSitemap :: Metadata -> Sitemap
packageMetadataSitemap m = do
    url' PackageR
    url' PackageSnapshotsR
  where
    url' floc = url $ floc $ metadataName m

tagSitemap :: Tag -> Sitemap
tagSitemap t = url $ TagR $ tagTag t


priority :: Double -> Route App -> Sitemap
priority p loc = yield $ SitemapUrl
    { sitemapLoc = loc
    , sitemapLastMod = Nothing
    , sitemapChangeFreq = Nothing
    , sitemapPriority = Just p
    }

url :: Route App -> Sitemap
url loc = yield $ SitemapUrl
    { sitemapLoc = loc
    , sitemapLastMod = Nothing
    , sitemapChangeFreq = Nothing
    , sitemapPriority = Nothing
    }
