module Handler.Sitemap (getSitemapR) where

import Import
import Yesod.Sitemap
import qualified Data.Conduit.List as CL
import qualified Control.Monad.State as State

type SitemapFor a = forall m. Monad m => Conduit a m (SitemapUrl (Route App))
type Sitemap = forall m. Monad m => Producer m (SitemapUrl (Route App))

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

    runDBSource $ do
        selectAll $= ltsSitemaps
        selectAll $= snapshotSitemaps
        selectAll $= packageMetadataSitemaps
        selectAll $= tagSitemaps


selectAll :: (PersistEntity val, PersistEntityBackend val ~ YesodPersistBackend App)
  => Source (YesodDB App) val
selectAll = selectSource [] [] $= CL.map entityVal

ltsSitemaps :: SitemapFor Lts
ltsSitemaps = sequenceConduits [ltsMajorSitemap, ltsSitemap] >> return ()

clNub :: (Monad m, Eq a) => Conduit a m a
clNub = evalStateC [] $ awaitForever $ \a -> do
    seen <- State.get
    unless (a `elem` seen) $ do
        State.put (a:seen)
        yield a

ltsMajorSitemap :: SitemapFor Lts
ltsMajorSitemap = CL.map ltsMajor =$= clNub =$= awaitForever go
  where
    go ver = priority 0.55 $ LtsR [pack (show ver)]

ltsSitemap :: SitemapFor Lts
ltsSitemap = awaitForever go
  where
    show' = pack . show
    go lts = url $ LtsR [slug]
      where
        slug = show' (ltsMajor lts) <> "." <> show' (ltsMinor lts)

snapshotSitemaps :: SitemapFor Stackage
snapshotSitemaps = awaitForever go
  where
    go s = do
        url' StackageHomeR
        url' StackageMetadataR
        url' StackageCabalConfigR
        url' StackageIndexR
        url' SnapshotPackagesR
        url' DocsR
        url' HoogleR
      where
        url' = url . SnapshotR (stackageSlug s)

packageMetadataSitemaps :: SitemapFor Metadata
packageMetadataSitemaps = awaitForever go
  where
    go m = do
        url' PackageR
        url' PackageSnapshotsR
      where
        url' floc = url $ floc $ metadataName m

tagSitemaps :: SitemapFor Tag
tagSitemaps = awaitForever go
  where
    go t = url $ TagR $ tagTag t


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
