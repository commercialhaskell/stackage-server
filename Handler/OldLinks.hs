module Handler.OldLinks
    ( getOldLtsR
    , getOldLtsMajorR
    , getOldNightlyR
    , getOldSnapshotR
    ) where

import Import
import Stackage.Database
import qualified Data.Text.Read as Reader

data LtsSuffix = LSMajor !Int
               | LSMinor !Int !Int

parseLtsSuffix :: Text -> Maybe LtsSuffix
parseLtsSuffix t0 = do
        Right (x, t1) <- Just $ Reader.decimal t0
        if null t1
            then return $ LSMajor x
            else do
                t2 <- stripPrefix "." t1
                Right (y, "") <- Just $ Reader.decimal t2
                return $ LSMinor x y

getOldLtsR :: [Text] -> Handler ()
getOldLtsR pieces = do
    (x, y, pieces') <- case pieces of
        t:ts | Just suffix <- parseLtsSuffix t -> do
            (x, y) <- case suffix of
                LSMajor x -> do
                    y <- newestLTSMajor x >>= maybe notFound return
                    return (x, y)
                LSMinor x y -> return (x, y)
            return (x, y, ts)
        _ -> do
            (x, y) <- newestLTS >>= maybe notFound return
            return (x, y, pieces)
    let name = concat ["lts-", tshow x, ".", tshow y]
    redirect $ concatMap (cons '/') $ name : pieces'

getOldLtsMajorR :: LtsMajor -> [Text] -> Handler ()
getOldLtsMajorR (LtsMajor x) pieces = do
    y <- newestLTSMajor x >>= maybe notFound return
    let name = concat ["lts-", tshow x, ".", tshow y]
    redirect $ concatMap (cons '/') $ name : pieces

getOldNightlyR :: [Text] -> Handler ()
getOldNightlyR pieces = do
    (day, pieces') <- case pieces of
        t:ts | Just day <- fromPathPiece t -> return (day, ts)
        _ -> do
            day <- newestNightly >>= maybe notFound return
            return (day, pieces)
    let name = "nightly-" ++ tshow day
    redirect $ concatMap (cons '/') $ name : pieces'

getOldSnapshotR :: Text -> [Text] -> Handler ()
getOldSnapshotR t ts =
    case fromPathPiece t :: Maybe SnapName of
        Just _ -> redirect $ concatMap (cons '/') $ t : ts
        Nothing -> notFound
