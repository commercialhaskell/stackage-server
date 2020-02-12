{-# LANGUAGE NoImplicitPrelude #-}
module Handler.StackageSdist
    ( getStackageSdistR
    , pnvToSnapshotPackageInfo
    ) where

import Import
import Stackage.Database
import Handler.Package (packagePage)

handlePNVTarball :: PackageNameP -> VersionP -> Handler TypedContent
handlePNVTarball name version =
    redirect $
    concat -- TODO: Should this be switched to HTTPS by now?
                -- unfortunately using insecure HTTP for cabal's sake
        [ "http://hackage.fpcomplete.com/package/"
        , toPathPiece name
        , "-"
        , toPathPiece version
        , ".tar.gz"
        ]


getStackageSdistR
  :: SnapName -> PackageNameVersion -> HandlerFor App TypedContent
getStackageSdistR sname pnv =
    track "Handler.StackageSdist.getStackageSdistR" $
    pnvToSnapshotPackageInfo sname pnv handlePNVTarball $ \isSameVersion spi ->
        if isSameVersion
            then packagePage (Just spi) (spiPackageName spi) >>= sendResponse
            else redirect $
                 SnapshotR sname $
                 StackageSdistR $ PNVNameVersion (spiPackageName spi) (spiVersion spi)


pnvToSnapshotPackageInfo ::
       SnapName
    -> PackageNameVersion
    -> (PackageNameP -> VersionP -> HandlerFor App b)
    -> (Bool -> SnapshotPackageInfo -> HandlerFor App b)
    -> HandlerFor App b
pnvToSnapshotPackageInfo sname pnv tarballHandler spiHandler =
    case pnv of
        PNVName pname -> spiHelper sname pname >>= spiHandler False
        PNVNameVersion pname version ->
            spiHelper sname pname >>= \spi -> spiHandler (version == spiVersion spi) spi
        PNVTarball name version -> tarballHandler name version


spiHelper :: SnapName -> PackageNameP -> Handler SnapshotPackageInfo
spiHelper sname pname = getSnapshotPackageInfo sname pname >>= maybe notFound return

