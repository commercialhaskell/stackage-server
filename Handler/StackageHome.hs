module Handler.StackageHome
    ( getStackageHomeR
    , getStackageCabalConfigR
    , getDocsR
    , getSnapshotPackagesR
    ) where

import Import
import Data.Time (FormatTime)
import qualified Database.Esqueleto as E
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
getStackageCabalConfigR slug = do
    error "getStackageCabalConfigR"
    {-
    Entity sid _ <- runDB $ getBy404 $ UniqueSnapshot slug
    render <- getUrlRender

    mdownload <- lookupGetParam "download"
    when (mdownload == Just "true") $
        addHeader "Content-Disposition" "attachment; filename=cabal.config"

    mglobal <- lookupGetParam "global"
    let isGlobal = mglobal == Just "true"

    respondSourceDB typePlain $ stream isGlobal render sid
  where
    stream isGlobal render sid =
        selectSource
            [ PackageStackage ==. sid
            ]
            [ Asc PackageName'
            , Asc PackageVersion
            ] $= (if isGlobal then conduitGlobal else conduitLocal) render

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
        toBuilder (toPathPiece slug) ++
        toBuilder ':' ++
        toBuilder (snapshotUrl render) ++
        toBuilder '\n'

    headerLocal render = yield $ Chunk $
        toBuilder (asText "-- Stackage snapshot from: ") ++
        toBuilder (snapshotUrl render) ++
        toBuilder (asText "\n-- Please place this file next to your .cabal file as cabal.config\n-- To only use tested packages, uncomment the following line:\n-- remote-repo: stackage-") ++
        toBuilder (toPathPiece slug) ++
        toBuilder ':' ++
        toBuilder (snapshotUrl render) ++
        toBuilder '\n'

    snapshotUrl render = asHttp $ render $ SnapshotR slug StackageHomeR

    asHttp (stripPrefix "http://" -> Just s) = "http://" <> s
    asHttp (stripPrefix "https://" -> Just s) = "http://" <> s
    asHttp (stripPrefix "//" -> Just s) = "http://" <> s
    asHttp s = error $ "Unexpected url prefix: " <> unpack s

    constraint p
        | Just True <- packageCore p = toBuilder $ asText " installed"
        | otherwise = toBuilder (asText " ==") ++
                      toBuilder (toPathPiece $ packageVersion p)

    showPackageGlobal (Entity _ p) =
        toBuilder (asText "constraint: ") ++
        toBuilder (toPathPiece $ packageName' p) ++
        constraint p ++
        toBuilder '\n'

    goFirst = do
        mx <- await
        forM_ mx $ \(Entity _ p) -> yield $ Chunk $
            toBuilder (asText "constraints: ") ++
            toBuilder (toPathPiece $ packageName' p) ++
            constraint p

    showPackageLocal (Entity _ p) =
        toBuilder (asText ",\n             ") ++
        toBuilder (toPathPiece $ packageName' p) ++
        constraint p
    -}

yearMonthDay :: FormatTime t => t -> String
yearMonthDay = formatTime defaultTimeLocale "%Y-%m-%d"

getSnapshotPackagesR :: SnapName -> Handler Html
getSnapshotPackagesR slug = do
    error "getSnapshotPackagesR"
    {-
    Entity sid _stackage <- runDB $ getBy404 $ UniqueSnapshot slug
    defaultLayout $ do
        setTitle $ toHtml $ "Package list for " ++ toPathPiece slug
        cachedWidget (20 * 60) ("package-list-" ++ toPathPiece slug) $ do
            packages' <- handlerToWidget $ runDB $ E.select $ E.from $ \(m,p) -> do
                E.where_ $
                    (m E.^. MetadataName E.==. p E.^. PackageName') E.&&.
                    (p E.^. PackageStackage E.==. E.val sid)
                E.orderBy [E.asc $ m E.^. MetadataName]
                E.groupBy ( m E.^. MetadataName
                          , m E.^. MetadataSynopsis
                          )
                return
                    ( m E.^. MetadataName
                    , m E.^. MetadataSynopsis
                    , E.max_ $ E.case_
                        [ ( p E.^. PackageHasHaddocks
                          , p E.^. PackageVersion
                          )
                        ]
                        (E.val (Version ""))
                    )
            let packages = flip map packages' $ \(name, syn, forceNotNull -> mversion) ->
                    ( E.unValue name
                    , mversion
                    , strip $ E.unValue syn
                    , (<$> mversion) $ \version -> HaddockR slug $ return $ concat
                        [ toPathPiece $ E.unValue name
                        , "-"
                        , version
                        ]
                    )
                forceNotNull (E.Value Nothing) = Nothing
                forceNotNull (E.Value (Just (Version v)))
                    | null v = Nothing
                    | otherwise = Just v
            $(widgetFile "package-list")
  where strip x = fromMaybe x (stripSuffix "." x)
        mback = Just (SnapshotR slug StackageHomeR, "Return to snapshot")
    -}

getDocsR :: SnapName -> Handler Html
getDocsR slug = do
    error "getDocsR"
    {-
    Entity sid _stackage <- runDB $ getBy404 $ UniqueSnapshot slug
    defaultLayout $ do
        setTitle $ toHtml $ "Module list for " ++ toPathPiece slug
        cachedWidget (20 * 60) ("module-list-" ++ toPathPiece slug) $ do
            modules' <- handlerToWidget $ runDB $ E.select $ E.from $ \(d,m) -> do
                E.where_ $
                    (d E.^. DocsSnapshot E.==. E.val (Just sid)) E.&&.
                    (d E.^. DocsId E.==. m E.^. ModuleDocs)
                E.orderBy [ E.asc $ m E.^. ModuleName
                          , E.asc $ d E.^. DocsName
                          ]
                return
                    ( m E.^. ModuleName
                    , m E.^. ModuleUrl
                    , d E.^. DocsName
                    , d E.^. DocsVersion
                    )
            let modules = flip map modules' $ \(name, url, package, version) ->
                    ( E.unValue name
                    , E.unValue url
                    , E.unValue package
                    , E.unValue version
                    )
            $(widgetFile "doc-list")
            -}
