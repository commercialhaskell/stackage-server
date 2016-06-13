{-# LANGUAGE QuasiQuotes #-}
module Handler.Hoogle where

import           Control.DeepSeq (NFData(..))
import           Control.DeepSeq.Generics (genericRnf)
import           Data.Data (Data)
import           Data.Text.Read (decimal)
import qualified Hoogle
import           Import
import           Text.Blaze.Html (preEscapedToHtml)
import Stackage.Database
import qualified Stackage.Database.Cron as Cron
import qualified Data.Text as T

getHoogleDB :: SnapName -> Handler (Maybe FilePath)
getHoogleDB name = track "Handler.Hoogle.getHoogleDB" $ do
    app <- getYesod
    liftIO $ Cron.getHoogleDB True (appHttpManager app) name

getHoogleR :: SnapName -> Handler Html
getHoogleR name = track "Handler.Hoogle.getHoogleR" $ do
    Entity _ snapshot <- lookupSnapshot name >>= maybe notFound return
    mquery <- lookupGetParam "q"
    mpage <- lookupGetParam "page"
    exact <- isJust <$> lookupGetParam "exact"
    mresults' <- lookupGetParam "results"
    let count' =
            case decimal <$> mresults' of
                Just (Right (i, "")) -> min perPage i
                _ -> perPage
        page =
            case decimal <$> mpage of
                Just (Right (i, "")) -> i
                _ -> 1
        offset = (page - 1) * perPage
    mdatabasePath <- getHoogleDB name
    dbPath <- maybe (hoogleDatabaseNotAvailableFor name) return mdatabasePath

    -- Avoid concurrent Hoogle queries, see
    -- https://github.com/fpco/stackage-server/issues/172
    lock <- appHoogleLock <$> getYesod
    HoogleQueryOutput results mtotalCount <-
      case mquery of
        Just query -> do
            let input = HoogleQueryInput
                    { hqiQueryInput = query
                    , hqiLimitTo = count'
                    , hqiOffsetBy = offset
                    , hqiExact = exact
                    }

            liftIO $ withMVar lock
                   $ const
                   $ Hoogle.withDatabase dbPath
                   -- NB! I got a segfault when I didn't force with $!
                   $ \db -> return $! runHoogleQuery db input
        Nothing -> return $ HoogleQueryOutput [] Nothing
    let queryText = fromMaybe "" mquery
        pageLink p = (SnapshotR name HoogleR
            , (if exact then (("exact", "true"):) else id)
            $ maybe id (\q' -> (("q", q'):)) mquery
              [("page", tshow p)])
        snapshotLink = SnapshotR name StackageHomeR
        hoogleForm = $(widgetFile "hoogle-form")
    defaultLayout $ do
        setTitle "Hoogle Search"
        $(widgetFile "hoogle")

getHoogleDatabaseR :: SnapName -> Handler Html
getHoogleDatabaseR name = track "Handler.Hoogle.getHoogleDatabaseR" $ do
    mdatabasePath <- getHoogleDB name
    case mdatabasePath of
        Nothing -> hoogleDatabaseNotAvailableFor name
        Just path -> sendFile "application/octet-stream" path

hoogleDatabaseNotAvailableFor :: SnapName -> Handler a
hoogleDatabaseNotAvailableFor name = track "Handler.Hoogle.hoogleDatabaseNotAvailableFor" $ do
    (>>= sendResponse) $ defaultLayout $ do
        setTitle "Hoogle database not available"
        [whamlet|
            <div .container>
                <p>The given Hoogle database is not available.
                <p>
                    <a href=@{SnapshotR name StackageHomeR}>Return to snapshot homepage
        |]

getPageCount :: Int -> Int
getPageCount totalCount = 1 + div totalCount perPage

perPage :: Int
perPage = 10

data HoogleQueryInput = HoogleQueryInput
    { hqiQueryInput  :: Text
    , hqiLimitTo     :: Int
    , hqiOffsetBy    :: Int
    , hqiExact       :: Bool
    }
    deriving (Eq, Read, Show, Data, Typeable, Ord, Generic)

data HoogleQueryOutput = HoogleQueryOutput [HoogleResult] (Maybe Int) -- ^ Int == total count
    deriving (Read, Typeable, Data, Show, Eq, Generic)
instance NFData HoogleQueryOutput where rnf = genericRnf

data HoogleResult = HoogleResult
    { hrURL     :: String
    , hrSources :: [(PackageLink, [ModuleLink])]
    , hrTitle   :: String -- ^ HTML
    , hrBody    :: String -- ^ plain text
    }
    deriving (Eq, Read, Show, Data, Typeable, Ord, Generic)

data PackageLink = PackageLink
    { plName :: String
    , plURL  :: String
    }
    deriving (Eq, Read, Show, Data, Typeable, Ord, Generic)

data ModuleLink = ModuleLink
    { mlName :: String
    , mlURL :: String
    }
    deriving (Eq, Read, Show, Data, Typeable, Ord, Generic)

instance NFData HoogleResult where rnf = genericRnf
instance NFData PackageLink where rnf = genericRnf
instance NFData ModuleLink where rnf = genericRnf

runHoogleQuery :: Hoogle.Database -> HoogleQueryInput -> HoogleQueryOutput
runHoogleQuery hoogledb HoogleQueryInput {..} =
    HoogleQueryOutput targets mcount
  where
    allTargets = Hoogle.searchDatabase hoogledb query
    targets = take (min 100 hqiLimitTo)
            $ drop hqiOffsetBy
            $ map fixResult allTargets
    query = unpack $ hqiQueryInput ++ if hqiExact then " is:exact" else ""

    mcount = limitedLength 0 allTargets

    limitedLength x [] = Just x
    limitedLength x (_:rest)
        | x >= 20 = Nothing
        | otherwise = limitedLength (x + 1) rest

    fixResult Hoogle.Target {..} = HoogleResult
        { hrURL     = targetURL
        , hrSources = toList $ do
            (pname, purl) <- targetPackage
            (mname, murl) <- targetModule
            let p = PackageLink pname purl
                m = ModuleLink mname murl
            Just (p, [m])
        , hrTitle   = -- FIXME find out why these replaces are necessary
                      unpack $ T.replace "<0>" "" $ T.replace "</0>" "" $ pack
                      targetItem
        , hrBody    = targetDocs
        }
