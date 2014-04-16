module Data.Hackage
    ( loadCabalFiles
    , sourceHackageSdist
    , createView
    , sourceHackageViewSdist
    ) where

import ClassyPrelude.Yesod hiding (get)
import Types
import Data.BlobStore
import Data.Conduit.Lazy (MonadActive (..), lazyConsume)
import Control.Monad.Logger (LoggingT)
import qualified Codec.Archive.Tar as Tar
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans.Resource (release)
import qualified Data.Text as T
import Data.Conduit.Zlib (ungzip, gzip)
import Text.XML.Cursor (($//), (&/), content, fromDocument, element, followingSibling)
import Text.HTML.DOM (sinkDoc)
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)
import System.IO (IOMode (ReadMode), openBinaryFile)
import Control.Monad.Catch (MonadCatch)
import Model (Uploaded (Uploaded))
import Filesystem (createTree)
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult (ParseOk))
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.PackageDescription (GenericPackageDescription, PackageDescription, packageDescription)
import Control.Exception (throw)
import Control.Monad.State (modify, put, get)
import Control.Concurrent.Lifted (fork)

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
            tarSource (Tar.read $ fromChunks bss) $$ parMapMC 32 go =$ sinkNull -- FIXME parMapM_C
  where
    withBinaryFile fp mode = bracket (liftIO $ openBinaryFile fp mode) (liftIO . hClose)

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

tarSource Tar.Done = return ()
tarSource (Tar.Fail e) = throwM e
tarSource (Tar.Next e es) = yield e >> tarSource es

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

sourceHackageViewSdist :: ( MonadIO m
                      , MonadThrow m
                      , MonadBaseControl IO m
                      , MonadResource m
                      , MonadReader env m
                      , HasHttpManager env
                      , HasHackageRoot env
                      , HasBlobStore env StoreKey
                      , MonadLogger m
                      , MonadActive m
                      )
                   => HackageView
                   -> PackageName
                   -> Version
                   -> m (Maybe (Source m ByteString))
sourceHackageViewSdist viewName name version = do
    let key = HackageViewSdist viewName name version
    msrc1 <- storeRead key
    case msrc1 of
        Just src -> return $ Just src
        Nothing -> do
            mcabalSrc <- storeRead $ HackageViewCabal viewName name version
            case mcabalSrc of
                Nothing -> return Nothing
                Just cabalSrc -> do
                    cabalLBS <- cabalSrc $$ sinkLazy
                    msrc <- sourceHackageSdist name version
                    case msrc of
                        Nothing -> return Nothing
                        Just src -> do
                            lbs <- fromChunks <$> lazyConsume (src $= ungzip)
                            let lbs' = Tar.write $ replaceCabal cabalLBS $ Tar.read lbs
                            sourceLazy lbs' $$ gzip =$ storeWrite key
                            storeRead key
  where
    cabalName = unpack $ concat
        [ toPathPiece name
        , "-"
        , toPathPiece version
        , "/"
        , toPathPiece name
        , ".cabal"
        ]

    replaceCabal _ Tar.Done = []
    replaceCabal _ (Tar.Fail e) = throw e -- עבירה גוררת עבירה
    replaceCabal lbs (Tar.Next e es) = replaceCabal' lbs e : replaceCabal lbs es

    replaceCabal' lbs e
        | Tar.entryPath e == cabalName = e { Tar.entryContent = Tar.NormalFile lbs (olength64 lbs) }
        | otherwise = e

createView :: ( MonadResource m
              , MonadCatch m
              , MonadReader env m
              , HasBlobStore env StoreKey
              , MonadBaseControl IO m
              , MonadLogger m
              )
           => HackageView
           -> (PackageName -> Version -> UTCTime -> GenericPackageDescription -> m GenericPackageDescription)
           -> Source m (Entity Uploaded)
           -> Sink ByteString m ()
           -> m ()
createView viewName modifyCabal src sink = withSystemTempDirectory "createview" $ \dir -> do
    $logDebug $ "Creating view: " ++ tshow viewName
    rels <- src $$ parMapMC 32 (uploadedConduit dir) =$ foldC
    entries <- liftIO $ Tar.pack dir (map fpToString $ setToList rels)
    sourceLazy (Tar.write entries) $$ gzip =$ sink
  where
    uploadedConduit dir (Entity _ (Uploaded name version time)) = do
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
                            return $ encodeUtf8 $ pack $ showGenericPackageDescription gpd'
                        _ -> return orig
                sourceLazy new $$ storeWrite (HackageViewCabal viewName name version)
                let fp = fpFromString dir </> relfp
                liftIO $ createTree $ directory fp
                writeFile fp new
                return $ asSet $ singletonSet relfp

-- FIXME put in conduit-combinators
parMapMC _ = mapMC
{- FIXME
parMapMC :: (MonadIO m, MonadBaseControl IO m)
         => Int
         -> (i -> m o)
         -> Conduit i m o
parMapMC threads f = evalStateC 0 $ do
    incoming <- liftIO $ newTBQueueIO $ threads * 8
    outgoing <- liftIO newTChanIO
    lift $ lift $ replicateM_ threads (addWorker incoming outgoing)
    awaitForever $ \x -> do
        cnt <- get
        ys <- atomically $ do
            writeTBQueue incoming (Just x)
            readWholeTChan outgoing
        put $ cnt + 1 - length ys
        yieldMany ys
    atomically $ writeTBQueue incoming Nothing
    let loop = do
            togo <- get
            when (togo > 0) $ do
                y <- atomically $ readTChan outgoing
                put $ togo - 1
                yield y
    loop
  where
    addWorker incoming outgoing =
        fork loop
      where
        loop = join $ atomically $ do
            mx <- readTBQueue incoming
            case mx of
                Nothing -> do
                    writeTBQueue incoming Nothing
                    return $ return ()
                Just x -> return $ do
                    y <- f x
                    atomically $ writeTChan outgoing y
                    loop

    readWholeTChan chan =
        go id
      where
        go front = do
            mx <- tryReadTChan chan
            case mx of
                Nothing -> return $ front []
                Just x -> go $ front . (x:)
-}
