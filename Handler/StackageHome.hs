module Handler.StackageHome where

import Import
import Data.Time (FormatTime)
import Data.Slug (SnapSlug)
import qualified Database.Esqueleto as E
import Handler.PackageList (cachedWidget)

getStackageHomeR :: SnapSlug -> Handler Html
getStackageHomeR slug = do
    stackage <- runDB $ do
        Entity _ stackage <- getBy404 $ UniqueSnapshot slug
        return stackage

    let minclusive =
            if "inclusive" `isSuffixOf` stackageTitle stackage
               then Just True
               else if "exclusive" `isSuffixOf` stackageTitle stackage
                       then Just False
                       else Nothing
        base = maybe 0 (const 1) minclusive :: Int
        hoogleForm =
            let queryText = "" :: Text
                exact = False
            in $(widgetFile "hoogle-form")
    Entity sid _stackage <- runDB $ getBy404 $ UniqueSnapshot slug
    defaultLayout $ do
        setTitle $ toHtml $ stackageTitle stackage
        cachedWidget (20 * 60) ("package-list-" ++ toPathPiece slug) $ do
            let maxPackages = 5000
            (packageListClipped, packages') <- handlerToWidget $ runDB $ do
                packages' <- E.select $ E.from $ \(m,p) -> do
                    E.where_ $
                        (m E.^. MetadataName E.==. p E.^. PackageName') E.&&.
                        (p E.^. PackageStackage E.==. E.val sid)
                    E.orderBy [E.asc $ m E.^. MetadataName]
                    E.groupBy ( m E.^. MetadataName
                              , m E.^. MetadataSynopsis
                              )
                    E.limit maxPackages
                    return
                        ( m E.^. MetadataName
                        , m E.^. MetadataSynopsis
                        , E.max_ (p E.^. PackageVersion)
                        , E.max_ $ E.case_
                            [ ( p E.^. PackageHasHaddocks
                              , p E.^. PackageVersion
                              )
                            ]
                            (E.val (Version ""))
                        )
                packageCount <- count [PackageStackage ==. sid]
                let packageListClipped = packageCount > maxPackages
                return (packageListClipped, packages')
            let packages = flip map packages' $ \(name, syn, latestVersion, forceNotNull -> mversion) ->
                    ( E.unValue name
                    , fmap unVersion $ E.unValue latestVersion
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
            $(widgetFile "stackage-home")
  where strip x = fromMaybe x (stripSuffix "." x)

getStackageMetadataR :: SnapSlug -> Handler TypedContent
getStackageMetadataR slug = do
    Entity sid _ <- runDB $ getBy404 $ UniqueSnapshot slug
    respondSourceDB typePlain $ do
        sendChunkBS "Override packages\n"
        sendChunkBS "=================\n"
        stream sid True
        sendChunkBS "\nPackages from Hackage\n"
        sendChunkBS   "=====================\n"
        stream sid False
  where
    stream sid isOverwrite =
        selectSource
            [ PackageStackage ==. sid
            , PackageOverwrite ==. isOverwrite
            ]
            [ Asc PackageName'
            , Asc PackageVersion
            ] $= mapC (Chunk . toBuilder . showPackage)

    showPackage (Entity _ p) = concat
        [ toPathPiece $ packageName' p
        , "-"
        , toPathPiece $ packageVersion p
        , "\n"
        ]

getStackageCabalConfigR :: SnapSlug -> Handler TypedContent
getStackageCabalConfigR slug = do
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

yearMonthDay :: FormatTime t => t -> String
yearMonthDay = formatTime defaultTimeLocale "%Y-%m-%d"

getOldStackageR :: PackageSetIdent -> [Text] -> Handler ()
getOldStackageR ident pieces = do
    Entity _ stackage <- runDB $ getBy404 $ UniqueStackage ident
    case parseRoute ("snapshot" : toPathPiece (stackageSlug stackage) : pieces, []) of
        Nothing -> notFound
        Just route -> redirect (route :: Route App)

getSnapshotPackagesR :: SnapSlug -> Handler Html
getSnapshotPackagesR slug = do
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

getDocsR :: SnapSlug -> Handler Html
getDocsR slug = do
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
