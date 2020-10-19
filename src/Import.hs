{-# LANGUAGE NoImplicitPrelude #-}
module Import
    ( module Import
    ) where

import ClassyPrelude.Yesod as Import hiding (getCurrentTime)
import Foundation as Import
import Settings as Import
import Settings.StaticFiles as Import
import Types as Import
import Yesod.Auth as Import
import Data.WebsiteContent as Import (WebsiteContent (..), Post (..))
import Data.Text.Read (decimal)
import RIO.Time (diffUTCTime, getCurrentTime)
--import qualified Prometheus as P
import Stackage.Database.Types (ModuleListingInfo(..))
import Formatting (format)
import Formatting.Time (diff)
import Yesod.GitRepo (grContent)

parseLtsPair :: Text -> Maybe (Int, Int)
parseLtsPair t1 = do
    (x, t2) <- either (const Nothing) Just $ decimal t1
    t3 <- stripPrefix "." t2
    (y, "") <- either (const Nothing) Just $ decimal t3
    Just (x, y)

packageUrl :: SnapName -> PackageNameP -> VersionP -> Route App
packageUrl sname pkgname pkgver = SnapshotR sname sdistR
  where
    sdistR = StackageSdistR (PNVNameVersion pkgname pkgver)

haddockUrl :: SnapName -> ModuleListingInfo -> Route App
haddockUrl sname mli =
    HaddockR
        sname
        [toPathPiece (mliPackageIdentifier mli), toPathPiece (mliModuleName mli) <> ".html"]

hoogleHaddockUrl :: SnapName -> PackageNameP -> ModuleNameP -> Route App
hoogleHaddockUrl sname pname mname = HaddockR sname [toPathPiece pname, toPathPiece mname <> ".html"]

track
    :: MonadIO m
    => String -> m a -> m a
track _ = id
{- FIXME prometheus isn't in Stackage anymore
track name inner = do
    start <- liftIO getCurrentTime
    result <- inner
    end <- liftIO getCurrentTime
    let latency = fromRational $ toRational (end `diffUTCTime` start) * 1000000
    liftIO (P.withLabel name (P.observe latency) duration)
    return result
  where
    {-# NOINLINE duration #-}
    duration :: P.Metric (P.Vector P.Label1 P.Histogram)
    duration =
        P.unsafeRegisterIO
            (P.vector
                 "fn"
                 (P.histogram
                      (P.Info
                           "stackage_server_fn"
                           "Stackage Server function call (duration in microseconds).")
                      P.defaultBuckets))
-}

dateDiff :: UTCTime -- ^ now
         -> Day -- ^ target
         -> LText
dateDiff (UTCTime now' _) target
    | now' == target = "today"
    | otherwise = format (diff True) $ diffUTCTime
        (UTCTime target 0)
        (UTCTime now' 0)

getPosts :: Handler (Vector Post)
getPosts = do
    now <- getCurrentTime
    posts <- getYesod >>= fmap wcPosts . liftIO . grContent . appWebsiteContent
    mpreview <- lookupGetParam "preview"
    case mpreview of
        Just "true" -> return posts
        _ -> return $ filter (\p -> postTime p <= now) posts

postYear :: Post -> Year
postYear p =
    let (y, _, _) = toGregorian $ utctDay $ postTime p
     in fromInteger y

postMonth :: Post -> Month
postMonth p =
    let (_, m, _) = toGregorian $ utctDay $ postTime p
     in Month m
