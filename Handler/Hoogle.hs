module Handler.Hoogle where

import           Control.DeepSeq (NFData(..))
import           Control.DeepSeq.Generics (genericRnf)
import           Control.Spoon (spoon)
import           Data.Data (Data (..))
import           Data.Text.Read (decimal)
import qualified Hoogle
import           Import
import           Text.Blaze.Html (preEscapedToHtml)
import Stackage.Database

getHoogleR :: SnapName -> Handler Html
getHoogleR slug = do
    error "getHoogleR"
    {- FIXME
    dirs <- getDirs
    mquery <- lookupGetParam "q"
    mpage <- lookupGetParam "page"
    exact <- maybe False (const True) <$> lookupGetParam "exact"
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
    Entity _ stackage <- runDB $ getBy404 $ UniqueSnapshot slug
    mdatabasePath <- getHoogleDB dirs stackage
    heDatabase <- case mdatabasePath of
        Just x -> return $ liftIO $ Hoogle.loadDatabase $ fpToString x
        Nothing -> hoogleDatabaseNotAvailableFor slug

    mresults <- case mquery of
        Just query -> runHoogleQuery heDatabase HoogleQueryInput
            { hqiQueryInput = query
            , hqiExactSearch = if exact then Just query else Nothing
            , hqiLimitTo = count'
            , hqiOffsetBy = offset
            }
        Nothing -> return $ HoogleQueryOutput "" [] Nothing
    let queryText = fromMaybe "" mquery
        pageLink p = (SnapshotR slug HoogleR
            , (if exact then (("exact", "true"):) else id)
            $ (maybe id (\q' -> (("q", q'):)) mquery)
              [("page", tshow p)])
        snapshotLink = SnapshotR slug StackageHomeR
        hoogleForm = $(widgetFile "hoogle-form")
    defaultLayout $ do
        setTitle "Hoogle Search"
        $(widgetFile "hoogle")
    -}

getHoogleDatabaseR :: SnapName -> Handler Html
getHoogleDatabaseR slug = do
    error "getHoogleDatabaseR"
    {-
    dirs <- getDirs
    Entity _ stackage <- runDB $ getBy404 $ UniqueSnapshot slug
    mdatabasePath <- getHoogleDB dirs stackage
    case mdatabasePath of
        Nothing -> hoogleDatabaseNotAvailableFor slug
        Just path -> sendFile "application/octet-stream" $ fpToString path

hoogleDatabaseNotAvailableFor :: SnapSlug -> Handler a
hoogleDatabaseNotAvailableFor slug = (>>= sendResponse) $ defaultLayout $ do
    setTitle "Hoogle database not available"
    [whamlet|
        <div .container>
            <p>The given Hoogle database is not available.
            <p>
                <a href=@{SnapshotR slug StackageHomeR}>Return to snapshot homepage
    |]

getPageCount :: Int -> Int
getPageCount totalCount = 1 + div totalCount perPage

perPage :: Int
perPage = 10

data HoogleQueryInput = HoogleQueryInput
    { hqiQueryInput  :: Text
    , hqiExactSearch :: Maybe Text
    , hqiLimitTo     :: Int
    , hqiOffsetBy    :: Int
    }
    deriving (Eq, Read, Show, Data, Typeable, Ord, Generic)

data HoogleQueryOutput = HoogleQueryBad Text
                       | HoogleQueryOutput Text [HoogleResult] (Maybe Int) -- ^ Text == HTML version of query, Int == total count
    deriving (Read, Typeable, Data, Show, Eq)

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

runHoogleQuery :: Monad m
               => m Hoogle.Database
               -> HoogleQueryInput
               -> m HoogleQueryOutput
runHoogleQuery heDatabase HoogleQueryInput {..} =
    runQuery $ Hoogle.parseQuery Hoogle.Haskell query
  where
    query = unpack hqiQueryInput

    runQuery (Left err) = return $ HoogleQueryBad (tshow err)
    runQuery (Right query') = do
        hoogledb <- heDatabase
        let query'' = Hoogle.queryExact classifier query'
            rawRes  = concatMap fixResult
                    $ Hoogle.search hoogledb query''
            mres    = spoon
                    $ take (min 100 hqiLimitTo)
                    $ drop hqiOffsetBy rawRes
            mcount  = spoon $ limitedLength 0 rawRes
            limitedLength x [] = Just x
            limitedLength x (_:rest)
                | x >= 100 = Nothing
                | otherwise = limitedLength (x + 1) rest
            rendered = pack $ Hoogle.showTagHTML $ Hoogle.renderQuery query''
        return $ case (,) <$> mres <*> mcount of
            Nothing ->
                HoogleQueryOutput rendered [] (Just 0)
            Just (results, mcount') ->
                HoogleQueryOutput rendered (take hqiLimitTo results) mcount'

    classifier = maybe Nothing
        (const (Just Hoogle.UnclassifiedItem))
        hqiExactSearch

    fixResult (_, Hoogle.Result locs self docs) = do
        (loc, _) <- take 1 locs
        let sources' = unionsWith (++) $
                mapMaybe (getPkgModPair . snd) locs
        return HoogleResult
            { hrURL     = loc
            , hrSources = mapToList sources'
            , hrTitle   = Hoogle.showTagHTML self
            , hrBody    = fromMaybe "Problem loading documentation" $
                              spoon $ Hoogle.showTagText docs
            }

    getPkgModPair :: [(String, String)]
                  -> Maybe (Map PackageLink [ModuleLink])
    getPkgModPair [(pkg, pkgname), (modu, moduname)] = do
        let pkg' = PackageLink pkgname pkg
            modu' = ModuleLink moduname modu
        return $ asMap $ singletonMap pkg' [modu']
    getPkgModPair _ = Nothing
    -}
