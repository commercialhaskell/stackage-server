module Handler.StackageHome where

import Data.BlobStore (storeExists)
import Import
import Data.Time (FormatTime)
import Data.Slug (SnapSlug)

getStackageHomeR :: SnapSlug -> Handler Html
getStackageHomeR slug = do
    muid <- maybeAuthId
    stackage <- runDB $ do
        Entity _ stackage <- getBy404 $ UniqueSnapshot slug
        return stackage
    let isOwner = muid == Just (stackageUser stackage)

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
        $(widgetFile "stackage-home")

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
    respondSourceDB typePlain $ stream sid
  where
    stream sid =
        selectSource
            [ PackageStackage ==. sid
            , PackageOverwrite ==. False
            ]
            [ Asc PackageName'
            , Asc PackageVersion
            ] $= (goFirst >> mapC (Chunk . showPackage))

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
