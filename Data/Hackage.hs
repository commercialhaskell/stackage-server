module Data.Hackage
    ( loadCabalFiles
    , sourceHackageSdist
    ) where

import ClassyPrelude.Yesod
import Types
import Data.BlobStore
import Data.Conduit.Lazy (MonadActive (..), lazyConsume)
import Control.Monad.Logger (LoggingT)
import qualified Codec.Archive.Tar as Tar
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans.Resource (release)
import qualified Data.Text as T
import Data.Conduit.Zlib (ungzip)
import Text.XML.Cursor (($//), (&/), content, fromDocument, element, followingSibling)
import Text.HTML.DOM (sinkDoc)
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)
import System.IO (IOMode (ReadMode), openBinaryFile)
import Control.Monad.Catch (MonadCatch)
import Model (Uploaded (Uploaded))
import Filesystem (createTree)
import Distribution.PackageDescription.Parse (showPackageDescription, parsePackageDescription, ParseResult (ParseOk))
import Distribution.PackageDescription (GenericPackageDescription, PackageDescription)

loadCabalFiles :: ( MonadActive m
                  , MonadBaseControl IO m
                  , MonadThrow m
                  , MonadIO m
                  , MonadReader env m
                  , HasHttpManager env
                  , HasBlobStore env StoreKey
                  , HasHackageRoot env
                  , MonadLogger m
                  , MonadCatch m
                  )
               => (PackageName -> Version -> m (Maybe UTCTime) -> m ()) -- ^ add upload
               -> m ()
loadCabalFiles addUpload = do
    HackageRoot root <- liftM getHackageRoot ask
    $logDebug $ "Entering loadCabalFiles, root == " ++ root
    req <- parseUrl $ unpack $ root ++ "/00-index.tar.gz"
    withSystemTempFile "hackage-index" $ \tempIndex handleOut -> do
        $logDebug $ "Requesting: " ++ tshow req
        withResponse req $ \res -> responseBody res $$ sinkHandle handleOut
        liftIO $ hClose handleOut
        withBinaryFile tempIndex ReadMode $ \handleIn -> do
            bss <- lazyConsume $ sourceHandle handleIn $= ungzip
            loop $ Tar.read $ fromChunks bss
  where
    withBinaryFile fp mode = bracket (liftIO $ openBinaryFile fp mode) (liftIO . hClose)

    loop (Tar.Next entry entries) = go entry >> loop entries
    loop Tar.Done = return ()
    loop (Tar.Fail e) = throwM e

    go entry = do
        case Tar.entryContent entry of
            Tar.NormalFile lbs _
                | Just (name, version) <- parseFilePath (Tar.entryPath entry) -> do
                    let key = HackageCabal name version
                    exists <- storeExists key
                    store <- liftM getBlobStore ask
                    unless exists $ withAcquire (storeWrite' store key) $ \sink ->
                        sourceLazy lbs $$ sink
                    setUploadDate name version addUpload
            _ -> return ()

setUploadDate :: ( MonadBaseControl IO m
                 , MonadThrow m
                 , MonadIO m
                 , MonadReader env m
                 , HasHttpManager env
                 , MonadLogger m
                 )
              => PackageName
              -> Version
              -> (PackageName -> Version -> m (Maybe UTCTime) -> m ())
              -> m ()
setUploadDate name version addUpload = addUpload name version $ do
    req <- parseUrl url
    $logDebug $ "Requesting: " ++ tshow req
    lbs <- withResponse req $ \res -> responseBody res $$ sinkLazy
    let uploadDateT = decodeUtf8 $ toStrict lbs
    return $ parseTime defaultTimeLocale "%c" $ unpack uploadDateT
  where
    url = unpack $ concat
        [ "http://hackage.haskell.org/package/"
        , toPathPiece name
        , "-"
        , toPathPiece version
        , "/upload-time"
        ]

    hasContent t c =
        if T.concat (c $// content) == t
            then [c]
            else []

parseFilePath :: String -> Maybe (PackageName, Version)
parseFilePath s =
    case filter (not . null) $ T.split (== '/') $ pack s of
        (name:version:_) -> Just (PackageName name, Version version)
        _ -> Nothing

sourceHackageSdist :: ( MonadIO m
                      , MonadThrow m
                      , MonadBaseControl IO m
                      , MonadResource m
                      , MonadReader env m
                      , HasHttpManager env
                      , HasHackageRoot env
                      , HasBlobStore env StoreKey
                      , MonadLogger m
                      )
                   => PackageName
                   -> Version
                   -> m (Maybe (Source m ByteString))
sourceHackageSdist name version = do
    let key = HackageSdist name version
    msrc1 <- storeRead key
    case msrc1 of
        Just src -> return $ Just src
        Nothing -> do
            HackageRoot root <- liftM getHackageRoot ask
            let url = concat
                        [ root
                        , "/"
                        , toPathPiece name
                        , "/"
                        , toPathPiece version
                        , "/"
                        , toPathPiece name
                        , "-"
                        , toPathPiece version
                        , ".tar.gz"
                        ]
            req' <- parseUrl $ unpack url
            let req = req' { checkStatus = \_ _ _ -> Nothing }
            $logDebug $ "Requesting: " ++ tshow req
            exists <- withResponse req $ \res ->
                if responseStatus res == status200
                    then do
                        responseBody res $$ storeWrite key
                        return True
                    else return False
            if exists
                then storeRead key
                else return Nothing

createView :: ( MonadResource m
              , MonadCatch m
              , MonadReader env m
              , HasBlobStore env StoreKey
              )
           => (PackageName -> Version -> UTCTime -> GenericPackageDescription -> m PackageDescription)
           -> Source m (Entity Uploaded)
           -> Sink ByteString m ()
           -> m ()
createView modifyCabal src sink = withSystemTempDirectory "createview" $ \dir -> do
    rels <- src $$ mapMC (\(Entity _ (Uploaded name version time)) -> do
        let relfp = fpFromText (toPathPiece name)
                </> fpFromText (toPathPiece version)
                </> fpFromText (concat
                        [ toPathPiece name
                        , "-"
                        , toPathPiece version
                        , ".cabal"
                        ])
        msrc <- storeRead $ HackageCabal name version
        case msrc of
            Nothing -> return mempty
            Just src -> do
                orig <- src $$ sinkLazy
                new <-
                    case parsePackageDescription $ unpack $ decodeUtf8 orig of
                        ParseOk _ gpd -> do
                            gpd' <- modifyCabal name version time gpd
                            return $ encodeUtf8 $ pack $ showPackageDescription gpd'
                        _ -> return orig
                let fp = fpFromString dir </> relfp
                liftIO $ createTree $ directory fp
                writeFile fp new
                return $ asSet $ singletonSet relfp
        ) =$ foldC
    entries <- liftIO $ Tar.pack dir (map fpToString $ setToList rels)
    sourceLazy (Tar.write entries) $$ sink

viewNoBounds :: Monad m
             => packageName -> version -> time
             -> GenericPackageDescription
             -> m GenericPackageDescription
viewNoBounds gpd = undefined
