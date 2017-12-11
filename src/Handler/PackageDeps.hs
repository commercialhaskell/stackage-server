module Handler.PackageDeps
  ( getPackageDepsR
  , getPackageRevDepsR
  , getSnapshotPackageDepsR
  , getSnapshotPackageRevDepsR
  ) where

import Import
import Stackage.Database

getPackageDepsR :: PackageName -> Handler Html
getPackageDepsR = packageDeps Nothing

getSnapshotPackageDepsR :: SnapName -> PackageNameVersion -> Handler Html
getSnapshotPackageDepsR snap (PNVNameVersion pname version) =
  packageDeps (Just (snap, version)) pname
getSnapshotPackageDepsR _ _ = notFound

packageDeps :: Maybe (SnapName, Version) -> PackageName -> Handler Html
packageDeps = helper Deps

getPackageRevDepsR :: PackageName -> Handler Html
getPackageRevDepsR = packageRevDeps Nothing

getSnapshotPackageRevDepsR :: SnapName -> PackageNameVersion -> Handler Html
getSnapshotPackageRevDepsR snap (PNVNameVersion pname version) =
  packageRevDeps (Just (snap, version)) pname
getSnapshotPackageRevDepsR _ _ = notFound

packageRevDeps :: Maybe (SnapName, Version) -> PackageName -> Handler Html
packageRevDeps = helper Revdeps

data DepType = Deps | Revdeps

helper :: DepType -> Maybe (SnapName, Version) -> PackageName -> Handler Html
helper depType mversion pname = track "Handler.PackageDeps.helper" $ do
  deps <-
    (case depType of
       Deps -> getDeps
       Revdeps -> getRevDeps) (toPathPiece pname) Nothing
  let packagePage =
        case mversion of
          Nothing -> PackageR pname
          Just (snap, version) -> SnapshotR snap $ StackageSdistR $ PNVNameVersion pname version
  defaultLayout $ do
    let title = toHtml $
          (case depType of
            Deps -> "Dependencies"
            Revdeps -> "Reverse dependencies ") ++ " for " ++ toPathPiece pname
    setTitle title
    [whamlet|
      <h1>#{title}
      <p>
        <a href=#{packagePage}>Return to package page
      <ul>
        $forall (name, range) <- deps
          <li>
            <a href=@{PackageR $ PackageName name} title=#{range}>#{name}
    |]
