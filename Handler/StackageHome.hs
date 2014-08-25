module Handler.StackageHome where

import Data.BlobStore (storeExists)
import Import

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

yearMonthDay = formatTime defaultTimeLocale "%Y-%m-%d"
