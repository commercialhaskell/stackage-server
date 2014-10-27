module Handler.Package where

import Import
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.), (&&.), Value (Value))
import Data.Time (addUTCTime)

getPackageR :: PackageName -> Handler Html
getPackageR pn = do
    let maxSnaps = 10
        asInt :: Int -> Int
        asInt = id
        haddocksLink ident version =
            HaddockR ident [concat [toPathPiece pn, "-", toPathPiece version]]
    (latestVersion, packages, downloads, recentDownloads) <- runDB $ do
        mupload <- selectFirst [UploadedName ==. pn] [Desc UploadedUploaded]
        Entity _ (Uploaded _ latestVersion _) <- maybe notFound return mupload
        packages <- E.select $ E.from $ \(p, s) -> do
            E.where_ $ (p ^. PackageStackage E.==. s ^. StackageId)
                   &&. (p ^. PackageName' E.==. E.val pn)
                   &&. (s ^. StackageTitle `E.like` E.val "%, exclusive")
            E.orderBy [E.desc $ s ^. StackageUploaded]
            E.limit maxSnaps
            --selectList [PackageName' ==. pn] [LimitTo 10, Desc PackageStackage]
            return (p ^. PackageVersion, s ^. StackageTitle, s ^. StackageIdent, s ^. StackageHasHaddocks)
        downloads <- count [DownloadPackage ==. pn]
        now <- liftIO getCurrentTime
        let nowMinus30 = addUTCTime (-30 * 24 * 60 * 60) now
        recentDownloads <- count [DownloadPackage ==. pn, DownloadTimestamp >=. nowMinus30]
        return (latestVersion, packages, downloads, recentDownloads)
    defaultLayout $ do
        setTitle $ toHtml pn
        $(combineStylesheets 'StaticR
            [ css_bootstrap_css
            , css_bootstrap_responsive_css
            ])
        $(widgetFile "package")
