{-# LANGUAGE ConstraintKinds #-}
module Handler.BuildPlan where

import Import hiding (get, PackageName (..), Version (..), DList)
import Data.Slug (SnapSlug)
import qualified Filesystem as F
import Data.Yaml (decodeFileEither)
import Control.Monad.State.Strict (get, modify, execStateT, MonadState)
import Control.Monad.Catch.Pure (runCatch)
import Stackage.Types
import Distribution.Package (PackageName (..))
import Data.Version (Version)
import Stackage.BuildPlan

getBuildPlanR :: SnapSlug -> Handler TypedContent
getBuildPlanR slug = do
    fullDeps <- (== Just "true") <$> lookupGetParam "full-deps"
    spec <- parseSnapshotSpec $ toPathPiece slug
    let set = setShellCommands simpleCommands
            $ setSnapshot spec
            $ setFullDeps fullDeps
              defaultSettings
    packages <- lookupGetParams "package" >>= mapM simpleParse
    when (null packages) $ invalidArgs ["Must provide at least one package"]
    toInstall <- liftIO $ getBuildPlan set packages
    selectRep $ do
        provideRep $ return $ toSimpleText toInstall
        provideRep $ return $ toJSON toInstall
        provideRepType "application/x-sh" $ return $ toShellScript set toInstall
