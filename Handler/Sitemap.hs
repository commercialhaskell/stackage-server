module Handler.Sitemap (getSitemapR) where

import Import
import Yesod.Sitemap
--import Stackage.Database

--type SitemapFor a = forall m. Monad m => Conduit a m (SitemapUrl (Route App))
type Sitemap = forall m. Monad m => Producer m (SitemapUrl (Route App))

getSitemapR :: Handler TypedContent
getSitemapR = track "Handler.Sitemap.getSitemapR" $ sitemap $ do
    priority 1.0 $ HomeR

    priority 0.9 $ OldSnapshotBranchR LtsBranch []
    priority 0.8 $ OldSnapshotBranchR NightlyBranch []

    priority 0.7 $ AllSnapshotsR
    priority 0.7 $ PackageListR

    priority 0.6 $ AuthorsR
    priority 0.6 $ InstallR
    priority 0.6 $ OlderReleasesR

{- FIXME
    runDBSource $ do
        --selectAll $= ltsSitemaps
        return () $= snapshotSitemaps -- FIXME
        return () $= packageMetadataSitemaps -- FIXME


selectAll :: (PersistEntity val, PersistEntityBackend val ~ YesodPersistBackend App)
  => Source (YesodDB App) val
selectAll = selectSource [] [] $= CL.map entityVal

clNub :: (Monad m, Eq a) => Conduit a m a
clNub = evalStateC [] $ awaitForever $ \a -> do
    seen <- State.get
    unless (a `elem` seen) $ do
        State.put (a:seen)
        yield a

ltsSitemaps :: SitemapFor Lts
ltsSitemaps = sequenceConduits [ltsMajorSitemap, ltsSitemap] >> return ()

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
-}

{-
snapshotSitemaps :: SitemapFor Snapshot
snapshotSitemaps = awaitForever go
  where
    go s = do
        url' StackageHomeR
        url' StackageCabalConfigR
        url' StackageIndexR
        url' SnapshotPackagesR
        url' DocsR
        url' HoogleR
      where
        url' = url . SnapshotR (snapshotName s)

packageMetadataSitemaps :: SitemapFor Package
packageMetadataSitemaps = awaitForever go
  where
    go m = do
        url' PackageR
        url' PackageSnapshotsR
      where
        url' floc = url $ floc $ PackageName $ packageName m


url :: Route App -> Sitemap
url loc = yield SitemapUrl
    { sitemapLoc = loc
    , sitemapLastMod = Nothing
    , sitemapChangeFreq = Nothing
    , sitemapPriority = Nothing
    }
-}

priority :: Double -> Route App -> Sitemap
priority p loc = yield SitemapUrl
    { sitemapLoc = loc
    , sitemapLastMod = Nothing
    , sitemapChangeFreq = Nothing
    , sitemapPriority = Just p
    }
