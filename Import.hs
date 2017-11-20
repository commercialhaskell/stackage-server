module Import
    ( module Import
    ) where

import ClassyPrelude.Yesod as Import
import Foundation as Import
import Settings as Import
import Settings.StaticFiles as Import
import Types as Import
import Yesod.Auth as Import
import Data.WebsiteContent as Import (WebsiteContent (..))
import Data.Text.Read (decimal)
import Data.Time.Clock (diffUTCTime)
import qualified Prometheus as P
import Stackage.Database (SnapName)
import Formatting (format)
import Formatting.Time (diff)

parseLtsPair :: Text -> Maybe (Int, Int)
parseLtsPair t1 = do
    (x, t2) <- either (const Nothing) Just $ decimal t1
    t3 <- stripPrefix "." t2
    (y, "") <- either (const Nothing) Just $ decimal t3
    Just (x, y)

packageUrl :: SnapName -> PackageName -> Version -> Route App
packageUrl sname pkgname pkgver = SnapshotR sname sdistR
  where
    sdistR = StackageSdistR (PNVNameVersion pkgname pkgver)

haddockUrl :: SnapName
           -> Text -- ^ package-version
           -> Text -- ^ module name
           -> Route App
haddockUrl sname pkgver name = HaddockR sname
    [ pkgver
    , omap toDash name ++ ".html"
    ]
  where
    toDash '.' = '-'
    toDash c = c

track
    :: MonadIO m
    => String -> m a -> m a
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

dateDiff :: UTCTime -- ^ now
         -> Day -- ^ target
         -> LText
dateDiff (UTCTime now' _) target
    | now' == target = "today"
    | otherwise = format (diff True) $ diffUTCTime
        (UTCTime target 0)
        (UTCTime now' 0)
