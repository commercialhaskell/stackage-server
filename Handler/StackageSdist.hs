module Handler.StackageSdist
    ( getStackageSdistR
    ) where

import Import
import Stackage.Database
import Handler.Package (packagePage)

getStackageSdistR :: SnapName -> PackageNameVersion -> Handler TypedContent
getStackageSdistR _ (PNVTarball name version) = do
    redirect $ concat
        -- unfortunately using insecure HTTP for cabal's sake
        [ "http://hackage.fpcomplete.com/package/"
        , toPathPiece name
        , "-"
        , toPathPiece version
        , ".tar.gz"
        ]
getStackageSdistR sname (PNVName pname) = do
    version <- versionHelper sname pname
    redirect $ SnapshotR sname $ StackageSdistR $ PNVNameVersion pname version
getStackageSdistR sname (PNVNameVersion pname version) = do
    version' <- versionHelper sname pname
    if version == version'
        then packagePage (Just (sname, version)) pname >>= sendResponse
        else redirect $ SnapshotR sname $ StackageSdistR $ PNVNameVersion pname version'

versionHelper sname pname = do
    Entity sid _ <- lookupSnapshot sname >>= maybe notFound return
    Entity _ sp <- lookupSnapshotPackage sid (toPathPiece pname) >>= maybe notFound return
    maybe notFound return $ fromPathPiece $ snapshotPackageVersion sp
