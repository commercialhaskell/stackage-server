module Handler.StackageHome where

import Data.BlobStore (storeExists)
import Import
import Data.Time (FormatTime)
import Data.Slug (SnapSlug)
import qualified Database.Esqueleto as E
import Handler.PackageList (cachedWidget)
import Stackage.ServerBundle (PackageDocs (..))
import Control.Monad.Writer.Strict (tell, execWriter)
import Stackage.BuildPlan (bpSystemInfo, bpPackages, ppVersion)
import Stackage.BuildConstraints (siCorePackages)
import Stackage.Prelude (display)

getStackageHomeR :: SnapSlug -> Handler Html
getStackageHomeR slug = do
    (Entity sid stackage, msi) <- getStackage slug

    hasBundle <- storeExists $ SnapshotBundle $ stackageIdent stackage
    let minclusive =
            if "inclusive" `isSuffixOf` stackageTitle stackage
               then Just True
               else if "exclusive" `isSuffixOf` stackageTitle stackage
                       then Just False
                       else Nothing
        base = maybe 0 (const 1) minclusive :: Int

    defaultLayout $ do
        setTitle $ toHtml $ stackageTitle stackage
        cachedWidget (20 * 60) ("package-list-" ++ toPathPiece slug) $ do
            let maxPackages = 5000
            (packageListClipped, packages') <- handlerToWidget $ runDB $ do
                packages' <- E.select $ E.from $ \(u,m,p) -> do
                    E.where_ $
                        (m E.^. MetadataName E.==. u E.^. UploadedName) E.&&.
                        (m E.^. MetadataName E.==. p E.^. PackageName') E.&&.
                        (p E.^. PackageStackage E.==. E.val sid)
                    E.orderBy [E.asc $ u E.^. UploadedName]
                    E.groupBy ( u E.^. UploadedName
                              , m E.^. MetadataSynopsis
                              )
                    E.limit maxPackages
                    return
                        ( u E.^. UploadedName
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
    (Entity sid _, msi) <- getStackage slug
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
    (Entity sid _, msi) <- getStackage slug
    render <- getUrlRender

    mdownload <- lookupGetParam "download"
    when (mdownload == Just "true") $
        addHeader "Content-Disposition" "attachment; filename=cabal.config"

    mglobal <- lookupGetParam "global"
    let isGlobal = mglobal == Just "true"

    respondSourceDB typePlain $
        stream (maybe (Left sid) Right msi) $=
        (if isGlobal then conduitGlobal else conduitLocal) render
  where
    stream (Left sid) =
        selectSource
            [ PackageStackage ==. sid
            ]
            [ Asc PackageName'
            , Asc PackageVersion
            ] $= mapC (\(Entity _ p) ->
                            ( toPathPiece $ packageName' p
                            , case packageCore p of
                                Just True -> Nothing
                                _ -> Just $ toPathPiece $ packageVersion p
                            ))
    stream (Right SnapshotInfo {..}) = forM_ (mapToList m) $ \(name, mversion) ->
        yield ( display name
              , display <$> mversion
              )
      where
        core = fmap (const Nothing) $ siCorePackages $ bpSystemInfo siPlan
        noncore = fmap (Just . ppVersion) $ bpPackages siPlan
        m = core ++ noncore

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
        toBuilder (render $ SnapshotR slug StackageHomeR) ++
        toBuilder (asText "\n-- Please place these contents in your global cabal config file.\n-- To only use tested packages, uncomment the following line\n-- and comment out other remote-repo lines:\n-- remote-repo: stackage-") ++
        toBuilder (toPathPiece slug) ++
        toBuilder ':' ++
        toBuilder (render $ SnapshotR slug StackageHomeR) ++
        toBuilder '\n'

    headerLocal render = yield $ Chunk $
        toBuilder (asText "-- Stackage snapshot from: ") ++
        toBuilder (render $ SnapshotR slug StackageHomeR) ++
        toBuilder (asText "\n-- Please place this file next to your .cabal file as cabal.config\n-- To only use tested packages, uncomment the following line:\n-- remote-repo: stackage-") ++
        toBuilder (toPathPiece slug) ++
        toBuilder ':' ++
        toBuilder (render $ SnapshotR slug StackageHomeR) ++
        toBuilder '\n'

    constraint Nothing = toBuilder $ asText " installed"
    constraint (Just version) =
                      toBuilder (asText " ==") ++
                      toBuilder (toPathPiece version)

    showPackageGlobal (name, mversion) =
        toBuilder (asText "constraint: ") ++
        toBuilder (toPathPiece name) ++
        constraint mversion ++
        toBuilder '\n'

    goFirst = do
        mx <- await
        forM_ mx $ \(name, mversion) -> yield $ Chunk $
            toBuilder (asText "constraints: ") ++
            toBuilder (toPathPiece name) ++
            constraint mversion

    showPackageLocal (name, mversion) =
        toBuilder (asText ",\n             ") ++
        toBuilder (toPathPiece name) ++
        constraint mversion

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
    (Entity sid _stackage, msi) <- getStackage slug
    defaultLayout $ do
        setTitle $ toHtml $ "Package list for " ++ toPathPiece slug
        cachedWidget (20 * 60) ("package-list-" ++ toPathPiece slug) $ do
            packages' <- handlerToWidget $ runDB $ E.select $ E.from $ \(u,m,p) -> do
                E.where_ $
                    (m E.^. MetadataName E.==. u E.^. UploadedName) E.&&.
                    (m E.^. MetadataName E.==. p E.^. PackageName') E.&&.
                    (p E.^. PackageStackage E.==. E.val sid)
                E.orderBy [E.asc $ u E.^. UploadedName]
                E.groupBy ( u E.^. UploadedName
                          , m E.^. MetadataSynopsis
                          )
                return
                    ( u E.^. UploadedName
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
    (Entity sid _stackage, msi) <- getStackage slug
    defaultLayout $ do
        setTitle $ toHtml $ "Module list for " ++ toPathPiece slug
        cachedWidget (20 * 60) ("module-list-" ++ toPathPiece slug) $ do
            modules <- handlerToWidget $ maybe (getFromDB sid) convertYaml msi
            $(widgetFile "doc-list")
  where
    getFromDB sid = do
        modules' <- runDB $ E.select $ E.from $ \(d,m) -> do
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
        return $ flip map modules' $ \(name, url, package, version) ->
                ( E.unValue name
                , E.unValue url
                , E.unValue package
                , E.unValue version
                )

    convertYaml :: SnapshotInfo -> Handler [(Text, Text, PackageName, Version)]
    convertYaml SnapshotInfo {..} = do
        render <- getUrlRender
        return $ sortBy comp $ ($ []) $ execWriter $ do
            forM_ (mapToList siDocMap) $ \(PackageName -> package, pd) -> do
                let version = Version $ pdVersion pd
                forM_ (mapToList $ pdModules pd) $ \(modname, path) -> do
                    let url = render $ HaddockR
                            slug
                            path
                    tell ((modname, url, package, version):)
      where
        comp (a, _, x, _) (b, _, y, _) = compare (a, x) (b, y)
