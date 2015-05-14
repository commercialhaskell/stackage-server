module Handler.StackageHome
    ( getStackageHomeR
    , getStackageCabalConfigR
    , getDocsR
    , getSnapshotPackagesR
    ) where

import Import
import Data.Time (FormatTime)
import Stackage.Database

getStackageHomeR :: SnapName -> Handler Html
getStackageHomeR name = do
    Entity sid snapshot <- lookupSnapshot name >>= maybe notFound return

    let hoogleForm =
            let queryText = "" :: Text
                exact = False
            in $(widgetFile "hoogle-form")
    defaultLayout $ do
        setTitle $ toHtml $ snapshotTitle snapshot
        packages <- getPackages sid
        $(widgetFile "stackage-home")
  where strip x = fromMaybe x (stripSuffix "." x)

getStackageCabalConfigR :: SnapName -> Handler TypedContent
getStackageCabalConfigR name = do
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
        toBuilder (snapshotUrl render) ++
        toBuilder (asText "\n-- Please place these contents in your global cabal config file.\n-- To only use tested packages, uncomment the following line\n-- and comment out other remote-repo lines:\n-- remote-repo: stackage-") ++
        toBuilder (toPathPiece name) ++
        toBuilder ':' ++
        toBuilder (snapshotUrl render) ++
        toBuilder '\n'

    headerLocal render = yield $ Chunk $
        toBuilder (asText "-- Stackage snapshot from: ") ++
        toBuilder (snapshotUrl render) ++
        toBuilder (asText "\n-- Please place this file next to your .cabal file as cabal.config\n-- To only use tested packages, uncomment the following line:\n-- remote-repo: stackage-") ++
        toBuilder (toPathPiece name) ++
        toBuilder ':' ++
        toBuilder (snapshotUrl render) ++
        toBuilder '\n'

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
getSnapshotPackagesR name = redirect $ SnapshotR name StackageHomeR

getDocsR :: SnapName -> Handler Html
getDocsR name = do
    Entity sid _ <- lookupSnapshot name >>= maybe notFound return
    mlis <- getSnapshotModules sid
    render <- getUrlRender
    let mliUrl mli = render $ haddockUrl name (mliPackageVersion mli) (mliName mli)
    defaultLayout $ do
        setTitle $ toHtml $ "Module list for " ++ toPathPiece name
        $(widgetFile "doc-list")
