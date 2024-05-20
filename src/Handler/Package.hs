{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Lists the package page similar to Hackage.

module Handler.Package
    ( getPackageR
    , getPackageSnapshotsR
    , packagePage
    , getPackageBadgeR
    , renderNumPackages
    ) where

import Control.Lens

import qualified RIO.Map as Map
import Data.Coerce
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Distribution.Package.ModuleForest
import Graphics.Badge.Barrier
import Import
import Stackage.Database
import Stackage.Database.PackageInfo (PackageInfo(..), Identifier(..), renderEmail)
import qualified Text.Blaze.Html.Renderer.Text as LT
import Yesod.GitRepo

-- | Page metadata package.
getPackageR :: PackageNameP -> Handler Html
getPackageR = track "Handler.Package.getPackageR" . packagePage Nothing

getPackageBadgeR :: PackageNameP -> SnapshotBranch -> Handler TypedContent
getPackageBadgeR pname branch = track "Handler.Package.getPackageBadgeR" $ do
    cacheSeconds (3 * 60 * 60)
    snapName     <- maybe notFound pure =<< newestSnapshot branch
    Entity sid _ <- maybe notFound pure =<< lookupSnapshot snapName
    mVersion <- getPackageVersionForSnapshot sid pname

    mLabel <- lookupGetParam "label"
    mStyle <- lookupGetParam "style"
    mColor <- lookupGetParam "color"
    let color = decodeColor mColor

    respond typeSvg $ case mStyle of
      Just "plastic"     -> renderStackageBadge plastic    mLabel snapName color mVersion
      Just "flat-square" -> renderStackageBadge flatSquare mLabel snapName color mVersion
      _                  -> renderStackageBadge flat       mLabel snapName color mVersion

decodeColor :: Maybe Text -> Color
decodeColor = \case
  Nothing -> brightgreen
  Just colorName ->
    case colorName of
      "brightgreen" -> brightgreen
      "green" -> green
      "yellow" -> yellow
      "yellowgreen" -> yellowgreen
      "orange" -> orange
      "red" -> red
      "blue" -> blue
      "gray" -> gray
      "lightgray" -> lightgray
      _ -> Color colorName

renderStackageBadge :: (Badge b, HasRightColor b)
                    => b          -- ^ Style
                    -> Maybe Text -- ^ Label
                    -> SnapName
                    -> Color
                    -> Maybe VersionP
                    -> LByteString
renderStackageBadge style mLabel snapName color = \case
    Nothing -> renderBadge (style & right .~ lightgray) badgeLabel "not available"
    Just v -> renderBadge (style & right .~ color) badgeLabel $ toPathPiece v
  where
    badgeLabel = fromMaybe ("stackage " <> badgeSnapName snapName) mLabel

    badgeSnapName (SNNightly _) = "nightly"
    badgeSnapName (SNLts x _)   = "lts-" <> tshow x

checkSpam :: PackageNameP -> Handler Html -> Handler Html
checkSpam pname inner = do
    wc <- getYesod >>= liftIO . grContent . appWebsiteContent
    if pname `member` wcSpamPackages wc
      then defaultLayout $ do
        setTitle $ "Spam package detected: " <> toHtml pname
        $(widgetFile "spam-package")
      else inner

packagePage :: Maybe SnapshotPackageInfo -> PackageNameP -> Handler Html
packagePage mspi pname =
    track "Handler.Package.packagePage" $
    checkSpam pname $ do
        mspi' <-
          case mspi of
            Just spi -> pure $ Just spi
            Nothing -> getSnapshotPackageLatestVersion pname
        case mspi' of
          Nothing -> do
            mhci <- run $ getHackageLatestVersion pname
            case mhci of
              Nothing -> notFound
              (Just hci) -> handlePackage $ Left hci
          Just spi -> handlePackage $ Right spi


handlePackage :: Either HackageCabalInfo SnapshotPackageInfo -> Handler Html
handlePackage epi = do
    (isDeprecated, inFavourOf, snapInfo, PackageInfo{..}) <- do
        (isDeprecated, inFavourOf) <- run $ getDeprecatedQuery pname
        snapInfo <- case epi of
          Right spi -> Right <$> run (getSnapshotPackagePageInfoQuery spi maxDisplayedDeps)
          Left hci -> pure $ Left hci
        pinfo <- run $ getPackageInfoQuery epi
        pure (isDeprecated, inFavourOf, snapInfo, pinfo)
    (msppi, mhciLatest) <- case snapInfo of
                             Left hci -> pure (Nothing, Just hci)
                             Right sppi -> pure (Just sppi, sppiLatestHackageCabalInfo sppi)
    let authors = enumerate piAuthors
        maintainers =
            let ms = enumerate piMaintainers
             in if ms == authors
                    then []
                    else ms
        mdisplayedVersion = msppi >>= sppiVersion
    defaultLayout $ do
        setTitle $ toHtml pname
        $(combineScripts 'StaticR [js_highlight_js])
        $(combineStylesheets 'StaticR [css_font_awesome_min_css, css_highlight_github_css])
        let hoogleForm name =
                let exact = False
                    mPackageName = Just pname
                    queryText = "" :: Text
                 in $(widgetFile "hoogle-form")
        $(widgetFile "package")
  where
    makeDepsLink spi f =
        SnapshotR (spiSnapName spi) $ f $ PNVNameVersion (spiPackageName spi) (spiVersion spi)
    pname = either hciPackageName spiPackageName epi
    enumerate = zip [0 :: Int ..]
    renderModules sppi = renderForest [] $ moduleForest $ coerce $ Map.keys modNames
      where
        modNames = sppiModuleNames sppi
        SnapshotPackageInfo{spiPackageName, spiVersion, spiSnapName} = sppiSnapshotPackageInfo sppi
        packageIdentifier = PackageIdentifierP spiPackageName spiVersion
        renderForest _ [] = mempty
        renderForest pathRev trees =
            [hamlet|<ul .docs-list>
                        $forall tree <- trees
                          ^{renderTree tree}
              |]
          where
            renderTree Node {..} =
                [hamlet|
                  <li>
                    $if isModule && hasDoc
                      <a href=@{haddockUrl spiSnapName mli}>#{modName}
                    $else
                      #{modName}
                    ^{renderForest pathRev' subModules}
                |]
              where
                mli = ModuleListingInfo modName packageIdentifier
                pathRev' = component : pathRev
                modName = moduleNameFromComponents (reverse pathRev')
                hasDoc = fromMaybe False $ Map.lookup modName modNames
    maxDisplayedDeps :: Int
    maxDisplayedDeps = 40

getPackageSnapshotsR :: PackageNameP -> Handler Html
getPackageSnapshotsR pn =
    track "Handler.Package.getPackageSnapshotsR" $ do
        cacheSeconds $ 60 * 60 * 24
        snapshots <- getSnapshotsForPackage pn (Just 50)
        defaultLayout
            (do setTitle ("Packages for " >> toHtml pn)
                $(combineStylesheets 'StaticR [css_font_awesome_min_css])
                $(widgetFile "package-snapshots"))

renderNumPackages :: Int -> Text
renderNumPackages n = T.pack $ show n ++ " package" ++ if n == 1 then "" else "s"
