{-# LANGUAGE NoImplicitPrelude #-}
module Handler.StackageSdist
    ( getStackageSdistR
    , pnvToSnapshotPackageInfo
    ) where

import Import
import Stackage.Database
import Handler.Package (packagePage)

getStackageSdistR
  :: SnapName -> PackageNameVersion -> HandlerFor App TypedContent
getStackageSdistR sname pnv =
    track "Handler.StackageSdist.getStackageSdistR" $
    pnvToSnapshotPackageInfo sname pnv $ \isSameVersion spi ->
        if isSameVersion
            then packagePage (Just spi) (spiPackageName spi) >>= sendResponse
            else redirect $
                 SnapshotR sname $
                 StackageSdistR $ PNVNameVersion (spiPackageName spi) (spiVersion spi)


pnvToSnapshotPackageInfo ::
       SnapName
    -> PackageNameVersion
    -> (Bool -> SnapshotPackageInfo -> HandlerFor App b)
    -> HandlerFor App b
pnvToSnapshotPackageInfo sname pnv spiHandler =
    case pnv of
        PNVName pname -> spiHelper sname pname >>= spiHandler False
        PNVNameVersion pname version ->
            spiHelper sname pname >>= \spi -> spiHandler (version == spiVersion spi) spi


spiHelper :: SnapName -> PackageNameP -> Handler SnapshotPackageInfo
spiHelper sname pname = getSnapshotPackageInfo sname pname >>= maybe notFound return
