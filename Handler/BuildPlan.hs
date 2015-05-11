{-# LANGUAGE ConstraintKinds #-}
module Handler.BuildPlan where

import Import hiding (get, PackageName (..), Version (..), DList)
import Data.Slug (SnapSlug)
import Stackage.Types
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
