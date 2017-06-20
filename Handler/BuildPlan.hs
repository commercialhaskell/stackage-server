{-# LANGUAGE ConstraintKinds #-}
module Handler.BuildPlan where

import Import hiding (get, PackageName (..), Version (..), DList)
import Stackage.Types
import Stackage.ShowBuildPlan
import Stackage.Database

getBuildPlanR :: SnapName -> Handler TypedContent
getBuildPlanR slug = track "Handler.BuildPlan.getBuildPlanR" $ do
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
