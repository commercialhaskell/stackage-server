module Handler.StackageHome where

import Data.BlobStore (storeExists)
import Import
import Data.Time (FormatTime)

getStackageHomeR :: PackageSetIdent -> Handler Html
getStackageHomeR ident = do
    (stackage, user) <- runDB $ do
        Entity _ stackage <- getBy404 $ UniqueStackage ident
        user <- get404 $ stackageUser stackage
        return (stackage, user)

    hasBundle <- storeExists $ SnapshotBundle ident
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

getStackageMetadataR :: PackageSetIdent -> Handler TypedContent
getStackageMetadataR ident = do
    Entity sid _ <- runDB $ getBy404 $ UniqueStackage ident
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

    showPackage (Entity _ (Package _ name version _)) = concat
        [ toPathPiece name
        , "-"
        , toPathPiece version
        , "\n"
        ]

getStackageCabalConfigR :: PackageSetIdent -> Handler TypedContent
getStackageCabalConfigR ident = do
    Entity sid _ <- runDB $ getBy404 $ UniqueStackage ident
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
        forM_ mx $ \(Entity _ (Package _ name version _)) -> yield $ Chunk $
            toBuilder (asText "constraints: ") ++
            toBuilder (toPathPiece name) ++
            toBuilder (asText " ==") ++
            toBuilder (toPathPiece version)

    showPackage (Entity _ (Package _ name version _)) =
        toBuilder (asText ",\n             ") ++
        toBuilder (toPathPiece name) ++
        toBuilder (asText " ==") ++
        toBuilder (toPathPiece version)

yearMonthDay :: FormatTime t => t -> String
yearMonthDay = formatTime defaultTimeLocale "%Y-%m-%d"
