{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Hoogle where

import qualified Data.Text as T
import Data.Text.Read (decimal)
import qualified Hoogle
import Import
import Stackage.Database
import Text.Blaze.Html (preEscapedToHtml)
import qualified Text.HTML.DOM
import Text.XML.Cursor (content, fromDocument, ($//))

getHoogleDB :: SnapName -> Handler (Maybe FilePath)
getHoogleDB name = track "Handler.Hoogle.getHoogleDB" do
    app <- getYesod
    liftIO $ appGetHoogleDB app name

getHoogleR :: SnapName -> Handler Html
getHoogleR name0 = track "Handler.Hoogle.getHoogleR" do
    let branch =
          case name0 of
            SNLts _ _ -> LtsBranch
            SNNightly _ -> NightlyBranch
    name <- newestSnapshot branch >>= maybe notFound return
    Entity _ snapshot <- lookupSnapshot name >>= maybe notFound return
    mquery <- lookupGetParam "q"
    mPackageName <- lookupGetParam "package"
    mpage <- lookupGetParam "page"
    exact <- isJust <$> lookupGetParam "exact"
    mresults' <- lookupGetParam "results"
    let count' =
            case decimal <$> mresults' of
                Just (Right (i, "")) -> min perPage i
                _                    -> perPage
        page =
            case decimal <$> mpage of
                Just (Right (i, "")) -> i
                _                    -> 1
        offset = (page - 1) * perPage
    mdatabasePath <- getHoogleDB name
    dbPath <- maybe (hoogleDatabaseNotAvailableFor name) return mdatabasePath

    urlRender <- getUrlRender
    HoogleQueryOutput results mtotalCount <-
      case mquery of
        Just query -> do
            let input = HoogleQueryInput
                    { hqiQueryInput =
                        case mPackageName of
                          Nothing -> query
                          Just pn -> concat ["+", pn, " ", query]
                    , hqiLimitTo = count'
                    , hqiOffsetBy = offset
                    , hqiExact = exact
                    }

            liftIO $ Hoogle.withDatabase dbPath
                   -- NB! I got a segfault when I didn't force with $!
                   $ \db -> return $! runHoogleQuery urlRender name db input
        Nothing -> return $ HoogleQueryOutput [] Nothing
    let queryText = fromMaybe "" mquery
        pageLink p = (SnapshotR name HoogleR
            , (if exact then (("exact", "true"):) else id)
            $ maybe id (\q' -> (("q", q'):)) mquery
              [("page", tshow p)])
        snapshotLink = SnapshotR name StackageHomeR
        hoogleForm = $(widgetFile "hoogle-form")
    defaultLayout do
        setTitle "Hoogle Search"
        $(widgetFile "hoogle")

getHoogleDatabaseR :: SnapName -> Handler Html
getHoogleDatabaseR name =
    track "Handler.Hoogle.getHoogleDatabaseR" do
        mdatabasePath <- getHoogleDB name
        case mdatabasePath of
            Nothing   -> hoogleDatabaseNotAvailableFor name
            Just path -> sendFile "application/octet-stream" path

hoogleDatabaseNotAvailableFor :: SnapName -> Handler a
hoogleDatabaseNotAvailableFor name =
    track "Handler.Hoogle.hoogleDatabaseNotAvailableFor" do
        sendResponse =<<
            defaultLayout
                (do setTitle "Hoogle database not available"
                    [whamlet|
            <div .container>
                <p>The given Hoogle database is not available.
                <p>
                    <a href=@{SnapshotR name StackageHomeR}>Return to snapshot homepage
        |])

getPageCount :: Int -> Int
getPageCount totalCount = 1 + div totalCount perPage

perPage :: Int
perPage = 10

data HoogleQueryInput = HoogleQueryInput
    { hqiQueryInput :: !Text
    , hqiLimitTo    :: !Int
    , hqiOffsetBy   :: !Int
    , hqiExact      :: !Bool
    }
    deriving (Eq, Show, Ord, Generic)

data HoogleQueryOutput = HoogleQueryOutput [HoogleResult] (Maybe Int) -- ^ Int == total count
    deriving (Show, Eq, Generic)
instance NFData HoogleQueryOutput

data HoogleResult = HoogleResult
    { hrURL     :: !Text
    , hrSources :: ![(PackageLink, [ModuleLink])]
    , hrTitle   :: !Text -- ^ HTML
    , hrBody    :: !String -- ^ plain text
    }
    deriving (Eq, Show, Ord, Generic)

data PackageLink = PackageLink
    { plName :: !PackageNameP
    , plURL  :: !Text
    }
    deriving (Eq, Show, Ord, Generic)

data ModuleLink = ModuleLink
    { mlName :: !ModuleNameP
    , mlURL  :: !Text
    }
    deriving (Eq, Show, Ord, Generic)

instance NFData HoogleResult
instance NFData PackageLink
instance NFData ModuleLink

runHoogleQuery :: (Route App -> Text)
               -> SnapName
               -> Hoogle.Database
               -> HoogleQueryInput
               -> HoogleQueryOutput
runHoogleQuery renderUrl snapshot hoogledb HoogleQueryInput {..} = HoogleQueryOutput targets mcount
  where
    allTargets = Hoogle.searchDatabase hoogledb query
    targets = take (min 100 hqiLimitTo) $ drop hqiOffsetBy $ map fixResult allTargets
    query =
        unpack $
        hqiQueryInput ++
        if hqiExact
            then " is:exact"
            else ""
    mcount = limitedLength 0 allTargets
    limitedLength x [] = Just x
    limitedLength x (_:rest)
        | x >= 20 = Nothing
        | otherwise = limitedLength (x + 1) rest
    fixResult target@Hoogle.Target {..} =
        HoogleResult
            { hrURL =
                  case sources of
                      [(_, [ModuleLink _ m])] -> m <> haddockAnchorFromUrl targetURL
                      _ -> fromMaybe (T.pack targetURL) $ asum [mModuleLink, mPackageLink]
            , hrSources = sources
            , hrTitle
               -- NOTE: from hoogle documentation:
               -- HTML span of the item, using 0 for the name and 1 onwards for arguments
               = T.replace "<0>" "" $ T.replace "</0>" "" $ pack targetItem
            , hrBody = targetDocs
            }
      where
        sources =
            toList do
                (packageLink, mkModuleUrl) <- targetLinks renderUrl snapshot target
                modName <- parseModuleNameP . fst =<< targetModule
                Just (packageLink, [ModuleLink modName $ mkModuleUrl modName])
        item =
            let doc = Text.HTML.DOM.parseLBS $ encodeUtf8 $ pack targetItem
                cursor = fromDocument doc
             in T.concat $ cursor $// content
        mModuleLink = do
            "module" <- Just targetType
            (_packageLink, mkModuleUrl) <- targetLinks renderUrl snapshot target
            modName <- parseModuleNameP . T.unpack =<< T.stripPrefix "module " item
            pure $ mkModuleUrl modName
        mPackageLink = do
            guard $ isNothing targetPackage
            "package" <- Just targetType
            pnameTxt <- T.stripPrefix "package " item
            pname <- fromPathPiece pnameTxt
            return $ renderUrl $ SnapshotR snapshot $ StackageSdistR $ PNVName pname
        haddockAnchorFromUrl = T.pack . ('#' :) . reverse . takeWhile (/= '#') . reverse

targetLinks ::
       (Route App -> Text)
    -> SnapName
    -> Hoogle.Target
    -> Maybe (PackageLink, ModuleNameP -> Text)
targetLinks renderUrl snapName Hoogle.Target {..} = do
    (pname, _) <- targetPackage
    packageName <- parsePackageNameP pname
    let mkModuleUrl modName = renderUrl (hoogleHaddockUrl snapName packageName modName)
    return (makePackageLink packageName, mkModuleUrl)

makePackageLink :: PackageNameP -> PackageLink
makePackageLink packageName = PackageLink packageName ("/package/" <> toPathPiece packageName)
