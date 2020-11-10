{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.PackageList where

import Import
import Text.Blaze

-- FIXME maybe just redirect to the LTS or nightly package list
getPackageListR :: Handler Html
getPackageListR = do
    sendResponseStatus status404 =<<
        defaultLayout
            (toWidget (preEscapedText
                 ("Page has been disabled, see: " <>
                   "<a href=\"https://github.com/fpco/stackage-server/issues/299\">" <>
                   "github:fpco/stackage-server#299</a>")))
  --   track "Handler.PackageList.getPackageListR" $
  --   defaultLayout $ do
  --       cacheSeconds $ 60 * 60 * 2
  --       setTitle "Package list"
  --       packages <- getAllPackages
  --       $(widgetFile "package-list")
  -- where
  --   strip x = fromMaybe x (stripSuffix "." x)
  --   makePackageLink snapName pli =
  --       SnapshotR snapName $ StackageSdistR $ PNVNameVersion (pliName pli) (pliVersion pli)
