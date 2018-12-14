{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.PackageDeps
  ( getPackageDepsR
  , getPackageRevDepsR
  , getSnapshotPackageDepsR
  , getSnapshotPackageRevDepsR
  ) where

import Handler.StackageSdist (pnvToSnapshotPackageInfo)
import Import
import Types (PackageVersionRev(..))
import Stackage.Database
import Stackage.Database.Types (SnapshotPackageInfo(..))

getPackageDepsR :: PackageNameP -> Handler Html
getPackageDepsR pname = do
    mspi <- getSnapshotPackageLatestVersion pname
    case mspi of
        Nothing -> redirect $ PackageR pname
        Just spi -> helper Deps spi

getSnapshotPackageDepsR :: SnapName -> PackageNameVersion -> Handler Html
getSnapshotPackageDepsR snapName pnv =
    pnvToSnapshotPackageInfo snapName pnv (\_ _ -> notFound) $ \isSameVersion spi ->
        if isSameVersion
            then helper Deps spi
            else redirect $
                 SnapshotR snapName $
                 SnapshotPackageDepsR $ PNVNameVersion (spiPackageName spi) (spiVersion spi)

getPackageRevDepsR :: PackageNameP -> Handler Html
getPackageRevDepsR pname = do
    mspi <- getSnapshotPackageLatestVersion pname
    case mspi of
        Nothing -> redirect $ PackageR pname
        Just spi -> helper RevDeps spi

getSnapshotPackageRevDepsR :: SnapName -> PackageNameVersion -> Handler Html
getSnapshotPackageRevDepsR snapName pnv =
    pnvToSnapshotPackageInfo snapName pnv (\_ _ -> notFound) $ \isSameVersion spi ->
        if isSameVersion
            then helper RevDeps spi
            else redirect $
                 SnapshotR snapName $
                 SnapshotPackageRevDepsR $ PNVNameVersion (spiPackageName spi) (spiVersion spi)


getPackagePageLink :: SnapName -> PackageVersionRev -> Route App
getPackagePageLink snapName (PackageVersionRev pname (VersionRev version _)) =
  SnapshotR snapName $ StackageSdistR $ PNVNameVersion pname version

data DepType = Deps | RevDeps

helper :: DepType -> SnapshotPackageInfo -> Handler Html
helper depType spi =
    track "Handler.PackageDeps.helper" $ do
        let (depsGetter, header) =
                case depType of
                    Deps -> (getForwardDeps, "Dependencies for ")
                    RevDeps -> (getReverseDeps, "Reverse dependencies on ")
        deps <- run $ depsGetter spi Nothing
        render <- getUrlRender
        let title =
                toHtml $
                header ++ toPathPiece (PackageIdentifierP (spiPackageName spi) (spiVersion spi))
            packagePageUrl =
                render $
                SnapshotR (spiSnapName spi) $
                StackageSdistR $ PNVNameVersion (spiPackageName spi) (spiVersion spi)
        defaultLayout $ do
            setTitle title
            [whamlet|
              <h1>#{title}
              <h3>There is a total of #{length deps} dependencies in <em>#{spiSnapName spi}</em>
              <p>
                <a href=#{packagePageUrl}>&lt;&lt; Return to package page
              <ul>
                $forall (depNameVerRev, verRange) <- deps
                  <li>
                    <a href=@{getPackagePageLink (spiSnapName spi) depNameVerRev} title="'#{spiPackageName spi}' version bounds: #{verRange}">#{depNameVerRev}
            |]
