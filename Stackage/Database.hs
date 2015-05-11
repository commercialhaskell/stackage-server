module Stackage.Database
    ( StackageDatabase
    , SnapName (..)
    , loadStackageDatabase
    , newestLTS
    , newestLTSMajor
    , newestNightly
    ) where

import ClassyPrelude.Conduit
import Data.Time
import Web.PathPieces
import Data.Text.Read (decimal)

data SnapName = SNLts !Int !Int
              | SNNightly !Day
    deriving (Eq, Read, Show)
instance PathPiece SnapName where
    toPathPiece (SNLts x y) = concat ["lts-", tshow x, ".", tshow y]
    toPathPiece (SNNightly d) = "nightly-" ++ tshow d

    fromPathPiece t0 =
        nightly <|> lts
      where
        nightly = stripPrefix "nightly-" t0 >>= readMay
        lts = do
            t1 <- stripPrefix "lts-" t0
            Right (x, t2) <- Just $ decimal t1
            t3 <- stripPrefix "." t2
            Right (y, "") <- Just $ decimal t3
            return $ SNLts x y

data StackageDatabase = StackageDatabase

loadStackageDatabase :: IO StackageDatabase
loadStackageDatabase = return StackageDatabase

newestLTS :: MonadIO m => StackageDatabase -> m (Maybe (Int, Int))
newestLTS _ = return $ Just (2, 8)

newestLTSMajor :: MonadIO m => StackageDatabase -> Int -> m (Maybe Int)
newestLTSMajor _ _ = return $ Just 7

newestNightly :: MonadIO m => StackageDatabase -> m (Maybe Day)
newestNightly _ = return $ Just $ fromGregorian 2015 4 3
