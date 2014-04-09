{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.SlugSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Slug
import ClassyPrelude.Yesod
import qualified System.Random.MWC as MWC
import Control.Monad.Reader (runReaderT)

spec :: Spec
spec = describe "Data.Slug" $ do
    prop "safeMakeSlug generates valid slugs" $ \(pack -> orig) -> do
        gen <- MWC.createSystemRandom
        slug <- runReaderT (safeMakeSlug orig False) gen
        mkSlug (unSlug slug) `shouldBe` Just slug
    prop "randomization works" $ \(pack -> orig) -> do
        gen <- MWC.createSystemRandom
        slug1 <- runReaderT (safeMakeSlug orig True) gen
        slug2 <- runReaderT (safeMakeSlug orig True) gen
        when (slug1 == slug2) $ error $ show (slug1, slug2)
