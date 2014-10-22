module Handler.Package where

import Import
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.), (&&.), Value (Value))

getPackageR :: PackageName -> Handler Html
getPackageR pn = do
    let maxSnaps = 10
        asInt :: Int -> Int
        asInt = id
        haddocksLink ident version =
            HaddockR ident [concat [toPathPiece pn, "-", toPathPiece version]]
    (latestVersion, packages) <- runDB $ do
        mupload <- selectFirst [UploadedName ==. pn] [Desc UploadedUploaded]
        Entity _ (Uploaded _ latestVersion _) <- maybe notFound return mupload
        packages <- E.select $ E.from $ \(p, s) -> do
            E.where_ $ (p ^. PackageStackage E.==. s ^. StackageId)
                   &&. (p ^. PackageName' E.==. E.val pn)
            E.orderBy [E.desc $ s ^. StackageUploaded]
            E.limit maxSnaps
            --selectList [PackageName' ==. pn] [LimitTo 10, Desc PackageStackage]
            return (p ^. PackageVersion, s ^. StackageTitle, s ^. StackageIdent, s ^. StackageHasHaddocks)
        return (latestVersion, packages)
    defaultLayout $ do
        setTitle $ toHtml pn
        $(widgetFile "package")
