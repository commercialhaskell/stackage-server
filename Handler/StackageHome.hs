module Handler.StackageHome
    ( getStackageHomeR
    , getStackageDiffR
    , getStackageCabalConfigR
    , getDocsR
    , getSnapshotPackagesR
    ) where

import Import
import Data.These
import Data.Time (FormatTime)
import Stackage.Database
import Stackage.Database.Types (isLts)
import Stackage.Snapshot.Diff

getStackageHomeR :: SnapName -> Handler TypedContent
getStackageHomeR name = track "Handler.StackageHome.getStackageHomeR" $ do
    Entity sid snapshot <- lookupSnapshot name >>= maybe notFound return
    previousSnapName <- fromMaybe name . map snd <$> snapshotBefore (snapshotName snapshot)
    let hoogleForm =
            let queryText = "" :: Text
                exact = False
                mPackageName = Nothing :: Maybe Text
            in $(widgetFile "hoogle-form")
    packageCount <- getPackageCount sid
    packages <- getPackages sid
    selectRep $ do
      provideRep $ do
        defaultLayout $ do
            setTitle $ toHtml $ snapshotTitle snapshot
            $(widgetFile "stackage-home")
      provideRep $ pure $ toJSON $ SnapshotInfo snapshot packages


  where strip x = fromMaybe x (stripSuffix "." x)

data SnapshotInfo
  = SnapshotInfo { snapshot :: Snapshot
                 , packages :: [PackageListingInfo]
                 }
instance ToJSON SnapshotInfo where
  toJSON SnapshotInfo{..} = object [ "snapshot" .= snapshot
                                   , "packages" .= packages
                                   ]

getStackageDiffR :: SnapName -> SnapName -> Handler TypedContent
getStackageDiffR name1 name2 = track "Handler.StackageHome.getStackageDiffR" $ do
    Entity sid1 _ <- lookupSnapshot name1 >>= maybe notFound return
    Entity sid2 _ <- lookupSnapshot name2 >>= maybe notFound return
    (map (snapshotName . entityVal) -> snapNames) <- getSnapshots Nothing 0 0
    let (ltsSnaps, nightlySnaps) = partition isLts $ reverse $ sort snapNames
    snapDiff <- getSnapshotDiff sid1 sid2
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle $ "Compare " ++ toHtml (toPathPiece name1) ++ " with "
                                  ++ toHtml (toPathPiece name2)
            $(widgetFile "stackage-diff")
        provideRep $ pure $ toJSON $ WithSnapshotNames name1 name2 snapDiff

getStackageCabalConfigR :: SnapName -> Handler TypedContent
getStackageCabalConfigR name = track "Handler.StackageHome.getStackageCabalConfigR" $ do
    Entity sid _ <- lookupSnapshot name >>= maybe notFound return
    render <- getUrlRender

    mdownload <- lookupGetParam "download"
    when (mdownload == Just "true") $
        addHeader "Content-Disposition" "attachment; filename=cabal.config"

    mglobal <- lookupGetParam "global"
    let isGlobal = mglobal == Just "true"

    plis <- getPackages sid

    respondSource typePlain $ yieldMany plis $=
        if isGlobal
            then conduitGlobal render
            else conduitLocal render
  where
    -- FIXME move this stuff into stackage-common
    conduitGlobal render = do
        headerGlobal render
        mapC (Chunk . showPackageGlobal)

    conduitLocal render = do
        headerLocal render
        goFirst
        mapC (Chunk . showPackageLocal)
        yield $ Chunk $ toBuilder '\n'

    headerGlobal render = yield $ Chunk $
        toBuilder (asText "-- Stackage snapshot from: ") ++
        toBuilder (oldSnapshotUrl render) ++
        toBuilder (asText "\n-- Please append these contents to the end of your global cabal config file.\n-- To only use tested packages, uncomment the following line\n-- and comment out other remote-repo lines:\n-- remote-repo: stackage-") ++
        toBuilder (toPathPiece name) ++
        toBuilder ':' ++
        toBuilder (snapshotUrl render) ++
        toBuilder '\n'

    headerLocal render = yield $ Chunk $
        toBuilder (asText "-- Stackage snapshot from: ") ++
        toBuilder (oldSnapshotUrl render) ++
        toBuilder (asText "\n-- Please place this file next to your .cabal file as cabal.config\n-- To only use tested packages, uncomment the following line:\n-- remote-repo: stackage-") ++
        toBuilder (toPathPiece name) ++
        toBuilder ':' ++
        toBuilder (snapshotUrl render) ++
        toBuilder '\n'

    oldSnapshotUrl render = asHttp $ render $ OldSnapshotR (toPathPiece name) []
    snapshotUrl render = asHttp $ render $ SnapshotR name StackageHomeR

    asHttp (stripPrefix "http://" -> Just s) = "http://" <> s
    asHttp (stripPrefix "https://" -> Just s) = "http://" <> s
    asHttp (stripPrefix "//" -> Just s) = "http://" <> s
    asHttp s = error $ "Unexpected url prefix: " <> unpack s

    constraint p
        | pliIsCore p = toBuilder $ asText " installed"
        | otherwise = toBuilder (asText " ==") ++
                      toBuilder (pliVersion p)

    showPackageGlobal p =
        toBuilder (asText "constraint: ") ++
        toBuilder (pliName p) ++
        constraint p ++
        toBuilder '\n'

    goFirst = do
        mx <- await
        forM_ mx $ \p -> yield $ Chunk $
            toBuilder (asText "constraints: ") ++
            toBuilder (pliName p) ++
            constraint p

    showPackageLocal p =
        toBuilder (asText ",\n             ") ++
        toBuilder (pliName p) ++
        constraint p

yearMonthDay :: FormatTime t => t -> String
yearMonthDay = formatTime defaultTimeLocale "%Y-%m-%d"

getSnapshotPackagesR :: SnapName -> Handler () -- FIXME move to OldLinks?
getSnapshotPackagesR name = track "Handler.StackageHome.getSnapshotPackagesR" $
    redirect $ SnapshotR name StackageHomeR

getDocsR :: SnapName -> Handler Html
getDocsR name = track "Handler.StackageHome.getDocsR" $ do
    Entity sid _ <- lookupSnapshot name >>= maybe notFound return
    mlis <- getSnapshotModules sid
    render <- getUrlRender
    let mliUrl mli = render $ haddockUrl name (mliPackageVersion mli) (mliName mli)
    defaultLayout $ do
        setTitle $ toHtml $ "Module list for " ++ toPathPiece name
        $(widgetFile "doc-list")
