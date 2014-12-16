module Handler.StackageHome where

import Data.BlobStore (storeExists)
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

    hasBundle <- storeExists $ SnapshotBundle $ stackageIdent stackage
    let minclusive =
            if "inclusive" `isSuffixOf` stackageTitle stackage
               then Just True
               else if "exclusive" `isSuffixOf` stackageTitle stackage
                       then Just False
                       else Nothing
        base = maybe 0 (const 1) minclusive :: Int
    Entity sid _stackage <- runDB $ getBy404 $ UniqueSnapshot slug
    defaultLayout $ do
        setTitle $ toHtml $ stackageTitle stackage
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
                    , E.max_ (p E.^. PackageVersion)
                    , E.max_ $ E.case_
                        [ ( p E.^. PackageHasHaddocks
                          , p E.^. PackageVersion
                          )
                        ]
                        (E.val (Version ""))
                    )
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

    showPackage (Entity _ (Package _ name version _ _)) = concat
        [ toPathPiece name
        , "-"
        , toPathPiece version
        , "\n"
        ]

getStackageCabalConfigR :: SnapSlug -> Handler TypedContent
getStackageCabalConfigR slug = do
    Entity sid _ <- runDB $ getBy404 $ UniqueSnapshot slug
    render <- getUrlRender

    mdownload <- lookupGetParam "download"
    when (mdownload == Just "true") $
        addHeader "Content-Disposition" "attachment; filename=cabal.config"

    respondSourceDB typePlain $ stream render sid
  where
    stream render sid =
        selectSource
            [ PackageStackage ==. sid
            , PackageOverwrite ==. False
            ]
            [ Asc PackageName'
            , Asc PackageVersion
            ] $= (header render >> goFirst >> mapC (Chunk . showPackage))

    header render = yield $ Chunk $
        toBuilder (asText "-- Stackage snapshot from: ") ++
        toBuilder (render $ SnapshotR slug StackageHomeR) ++
        toBuilder (asText "\n-- Please place this file next to your .cabal file as cabal.config\n-- To only use tested packages, uncomment the following line:\n-- remote-repo: stackage-") ++
        toBuilder (toPathPiece slug) ++
        toBuilder ':' ++
        toBuilder (render $ SnapshotR slug StackageHomeR) ++
        toBuilder '\n'
    goFirst = do
        mx <- await
        forM_ mx $ \(Entity _ (Package _ name version _ _)) -> yield $ Chunk $
            toBuilder (asText "constraints: ") ++
            toBuilder (toPathPiece name) ++
            toBuilder (asText " ==") ++
            toBuilder (toPathPiece version)

    showPackage (Entity _ (Package _ name version _ _)) =
        toBuilder (asText ",\n             ") ++
        toBuilder (toPathPiece name) ++
        toBuilder (asText " ==") ++
        toBuilder (toPathPiece version)

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
