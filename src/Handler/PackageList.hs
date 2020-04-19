{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.PackageList where

import Import
import Stackage.Database


-- FIXME maybe just redirect to the LTS or nightly package list
getPackageListR :: Handler Html
getPackageListR =
    track "Handler.PackageList.getPackageListR" $
    defaultLayout $ do
        cacheSeconds $ 60 * 60 * 2
        setTitle "Package list"
        packages <- getAllPackages
        $(widgetFile "package-list")
  where
    strip x = fromMaybe x (stripSuffix "." x)
    makePackageLink snapName pli =
        SnapshotR snapName $ StackageSdistR $ PNVNameVersion (pliName pli) (pliVersion pli)
