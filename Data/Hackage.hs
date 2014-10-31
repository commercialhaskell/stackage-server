module Data.Hackage
    ( loadCabalFiles
    , sourceHackageSdist
    , createView
    , sourceHackageViewSdist
    , sinkUploadHistory
    , UploadState (..)
    , UploadHistory
    , sourceHistory
    ) where

import ClassyPrelude.Yesod hiding (get)
import Types
import Data.BlobStore
import Data.Conduit.Lazy (MonadActive (..), lazyConsume)
import qualified Codec.Archive.Tar as Tar
import Control.Monad.Reader (MonadReader, ask, runReaderT)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Text as T
import Data.Conduit.Zlib (ungzip, gzip)
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory)
import System.IO (IOMode (ReadMode), openBinaryFile)
import Control.Monad.Catch (MonadMask)
import Model (Uploaded (Uploaded), Metadata (..))
import Filesystem (createTree)
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult (ParseOk))
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.PackageDescription (GenericPackageDescription)
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Package as PD
import Control.Exception (throw)
import Control.Monad.State.Strict (put, get, execStateT, MonadState)
import Crypto.Hash.Conduit (sinkHash)
import Crypto.Hash (Digest, SHA256)
import Data.Byteable (toBytes)
import Distribution.Text (display)
import Text.Markdown (Markdown (Markdown))
import Data.Foldable (foldMap)
import qualified Data.Traversable as T

sinkUploadHistory :: Monad m => Consumer (Entity Uploaded) m UploadHistory
sinkUploadHistory =
    foldlC go mempty
  where
    go history (Entity _ (Uploaded name version time)) =
        case lookup name history of
            Nothing -> insertMap name (singletonMap version time) history
            Just vhistory -> insertMap name (insertMap version time vhistory) history

loadCabalFiles :: ( MonadActive m
                  , MonadBaseControl IO m
                  , MonadThrow m
                  , MonadIO m
                  , MonadReader env m
                  , HasHttpManager env
                  , HasBlobStore env StoreKey
                  , HasHackageRoot env
                  , MonadLogger m
                  , MonadMask m
                  )
               => UploadHistory -- ^ initial
               -> HashMap PackageName (Version, ByteString)
               -> m (UploadState Metadata)
loadCabalFiles uploadHistory0 metadata0 = (>>= runUploadState) $ flip execStateT (UploadState uploadHistory0 [] metadata0 mempty) $ do
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
                    -- It's not longer sufficient to simply check if the cabal
                    -- file exists, since Hackage now allows updating in place.
                    -- Instead, we have to check if it matches what we have
                    -- and, if not, update it.
                    store <- liftM getBlobStore ask
                    newDigest :: Digest SHA256 <- sourceLazy lbs $$ sinkHash
                    toStore <- withAcquire (storeRead' store key) $ \mcurr ->
                        case mcurr of
                            Nothing -> return True
                            Just curr -> do
                                -- Check if it matches. This is cheaper than
                                -- always writing, since it can take advantage
                                -- of the local filesystem cache and not go to
                                -- S3 each time.
                                currDigest <- curr $$ sinkHash
                                return $! currDigest /= newDigest
                    when toStore $ withAcquire (storeWrite' store key) $ \sink ->
                        sourceLazy lbs $$ sink
                    setUploadDate name version

                    setMetadata name version (toBytes newDigest)
                              $ parsePackageDescription $ unpack $ decodeUtf8 lbs
            _ -> return ()

runUploadState :: MonadIO m => UploadState (IO a) -> m (UploadState a)
runUploadState (UploadState w x y z) = liftIO $ UploadState w x y <$> T.sequence z

tarSource :: (Exception e, MonadThrow m)
          => Tar.Entries e
          -> Producer m Tar.Entry
tarSource Tar.Done = return ()
tarSource (Tar.Fail e) = throwM e
tarSource (Tar.Next e es) = yield e >> tarSource es

type UploadHistory = HashMap PackageName (HashMap Version UTCTime)
data UploadState md = UploadState
    { usHistory :: !UploadHistory
    , usChanges :: ![Uploaded]
    , usMetadata :: !(HashMap PackageName (Version, ByteString))
    , usMetaChanges :: !(HashMap PackageName md)
    }

setUploadDate :: ( MonadBaseControl IO m
                 , MonadThrow m
                 , MonadIO m
                 , MonadReader env m
                 , MonadState (UploadState (IO Metadata)) m
                 , HasHttpManager env
                 , MonadLogger m
                 )
              => PackageName
              -> Version
              -> m ()
setUploadDate name version = do
    UploadState history changes us3 us4 <- get
    case lookup name history >>= lookup version of
        Just _ -> return ()
        Nothing -> do
            req <- parseUrl url
            $logDebug $ "Requesting: " ++ tshow req
            lbs <- withResponse req $ \res -> responseBody res $$ sinkLazy
            let uploadDateT = decodeUtf8 $ toStrict lbs
            case parseTime defaultTimeLocale "%c" $ unpack uploadDateT of
                Nothing -> return ()
                Just time -> do
                    let vhistory = insertMap version time $ fromMaybe mempty $ lookup name history
                        history' = insertMap name vhistory history
                        changes' = Uploaded name version time : changes
                    put $ UploadState history' changes' us3 us4
  where
    url = unpack $ concat
        [ "http://hackage.haskell.org/package/"
        , toPathPiece name
        , "-"
        , toPathPiece version
        , "/upload-time"
        ]

setMetadata :: ( MonadBaseControl IO m
               , MonadThrow m
               , MonadIO m
               , MonadReader env m
               , MonadState (UploadState (IO Metadata)) m
               , HasHttpManager env
               , MonadLogger m
               , MonadActive m
               , HasBlobStore env StoreKey
               , HasHackageRoot env
               )
            => PackageName
            -> Version
            -> ByteString
            -> ParseResult PD.GenericPackageDescription
            -> m ()
setMetadata name version hash' gpdRes = do
    UploadState us1 us2 mdMap mdChanges <- get
    let toUpdate =
            case lookup name mdMap of
                Just (currVersion, currHash) ->
                    case compare currVersion version of
                        LT -> True
                        GT -> False
                        EQ -> currHash /= hash'
                Nothing -> True
    if toUpdate
        then case gpdRes of
                ParseOk _ gpd -> do
                    !md <- getMetadata name version hash' gpd
                    put $! UploadState us1 us2
                                (insertMap name (version, hash') mdMap)
                                (insertMap name md mdChanges)
                _ -> return ()
        else return ()

getMetadata :: ( MonadActive m
               , MonadIO m
               , MonadBaseControl IO m
               , MonadThrow m
               , MonadReader env m
               , HasBlobStore env StoreKey
               , HasHackageRoot env
               , HasHttpManager env
               , MonadLogger m
               )
            => PackageName
            -> Version
            -> ByteString
            -> PD.GenericPackageDescription
            -> m (IO Metadata)
getMetadata name version hash' gpd = do
    let pd = PD.packageDescription gpd
    env <- ask
    return $ liftIO $ runNoLoggingT $ flip runReaderT env $ do
        (mreadme, mchangelog, mlicenseContent) <-
            grabExtraFiles name version
#if MIN_VERSION_Cabal(1, 20, 0)
                $ PD.licenseFiles pd
#else
                [PD.licenseFile pd]
#endif
        return Metadata
            { metadataName = name
            , metadataVersion = version
            , metadataHash = hash'
            , metadataDeps = setToList
                           $ asSet
                           $ concat
                  [ foldMap goTree $ PD.condLibrary gpd
                  , foldMap (goTree . snd) $ PD.condExecutables gpd
                  ]
            , metadataAuthor = pack $ PD.author pd
            , metadataMaintainer = pack $ PD.maintainer pd
            , metadataLicenseName = pack $ display $ PD.license pd
            , metadataHomepage = pack $ PD.homepage pd
            , metadataBugReports = pack $ PD.bugReports pd
            , metadataSynopsis = pack $ PD.synopsis pd
            , metadataSourceRepo = mapMaybe showSourceRepo $ PD.sourceRepos pd
            , metadataCategory = pack $ PD.category pd
            , metadataLibrary = isJust $ PD.library pd
            , metadataExes = length $ PD.executables pd
            , metadataTestSuites = length $ PD.testSuites pd
            , metadataBenchmarks = length $ PD.benchmarks pd
            , metadataReadme = fromMaybe (toHtml $ Textarea $ pack $ PD.description pd) mreadme
            , metadataChangelog = mchangelog
            , metadataLicenseContent = mlicenseContent
            }
  where
    goTree (PD.CondNode _ deps comps) = concatMap goDep deps ++ concatMap goComp comps
    goDep (PD.Dependency (PD.PackageName n) _) = singletonSet $ pack n
    goComp (_, tree, mtree) = goTree tree ++ maybe mempty goTree mtree

showSourceRepo :: PD.SourceRepo -> Maybe Text
showSourceRepo = fmap pack . PD.repoLocation

grabExtraFiles :: ( MonadActive m
                  , MonadIO m
                  , MonadBaseControl IO m
                  , MonadThrow m
                  , MonadReader env m
                  , HasBlobStore env StoreKey
                  , HasHackageRoot env
                  , HasHttpManager env
                  , MonadLogger m
                  )
               => PackageName
               -> Version
               -> [String] -- ^ license files
               -> m (Maybe Html, Maybe Html, Maybe Html) -- ^ README, changelog, license
grabExtraFiles name version lfiles = runResourceT $ do
    msrc <- sourceHackageSdist name version
    handle (\(_ :: Tar.FormatError) -> return (Nothing,Nothing,Nothing)) $
        case msrc of
            Nothing -> return mempty
            Just src -> do
                bss <- lazyConsume $ src $= ungzip
                tarSource (Tar.read $ fromChunks bss) $$ foldlC go mempty
  where
    go trip@(mreadme, mchangelog, mlicense) entry =
        case Tar.entryContent entry of
            Tar.NormalFile lbs _ ->
                let name' = drop 1 $ dropWhile (/= '/') $ Tar.entryPath entry in
                case toLower name' of
                    "readme.md"     -> (md lbs, mchangelog, mlicense)
                    "readme"        -> (txt lbs, mchangelog, mlicense)
                    "readme.txt"    -> (txt lbs, mchangelog, mlicense)
                    "changelog.md"  -> (mreadme, md lbs, mlicense)
                    "changelog"     -> (mreadme, txt lbs, mlicense)
                    "changelog.txt" -> (mreadme, txt lbs, mlicense)
                    _ | name' `elem` lfiles -> (mreadme, mchangelog, txt lbs)
                    _ -> trip
            _ -> trip

    md = Just . toHtml . Markdown . decodeUtf8
    txt = Just . toHtml . Textarea . toStrict . decodeUtf8

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
                        , "/package/"
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
              , MonadMask m
              , MonadReader env m
              , HasBlobStore env StoreKey
              , MonadBaseControl IO m
              , MonadLogger m
              )
           => HackageView
           -> (PackageName -> Version -> UTCTime -> GenericPackageDescription -> m GenericPackageDescription)
           -> Source m Uploaded
           -> Sink ByteString m ()
           -> m ()
createView viewName modifyCabal src sink = withSystemTempDirectory "createview" $ \dir -> do
    $logDebug $ "Creating view: " ++ tshow viewName
    rels <- src $$ parMapMC 32 (uploadedConduit dir) =$ foldC
    entries <- liftIO $ Tar.pack dir (map fpToString $ setToList rels)
    sourceLazy (Tar.write entries) $$ gzip =$ sink
  where
    uploadedConduit dir (Uploaded name version time) = do
        let relfp = fpFromText (toPathPiece name)
                </> fpFromText (toPathPiece version)
                </> fpFromText (concat
                        [ toPathPiece name
                        , "-"
                        , toPathPiece version
                        , ".cabal"
                        ])
            fp = fpFromString dir </> relfp
            key = HackageViewCabal viewName name version
        mprev <- storeRead key
        case mprev of
            Just src' -> do
                liftIO $ createTree $ directory fp
                src' $$ sinkFile fp
                return $ asSet $ singletonSet relfp
            Nothing -> do
                msrc <- storeRead $ HackageCabal name version
                case msrc of
                    Nothing -> return mempty
                    Just src' -> do
                        orig <- src' $$ sinkLazy
                        new <-
                            case parsePackageDescription $ unpack $ decodeUtf8 orig of
                                ParseOk _ gpd -> do
                                    gpd' <- modifyCabal name version time gpd
                                    let str = showGenericPackageDescription gpd'
                                    -- sanity check
                                    case parsePackageDescription str of
                                        ParseOk _ _ -> return $ encodeUtf8 $ pack str
                                        x -> do
                                            $logError $ "Created cabal file that could not be parsed: " ++ tshow (x, str)
                                            return orig
                                _ -> return orig
                        sourceLazy new $$ storeWrite key
                        liftIO $ createTree $ directory fp
                        writeFile fp new
                        return $ asSet $ singletonSet relfp

sourceHistory :: Monad m => UploadHistory -> Producer m Uploaded
sourceHistory =
    mapM_ go . mapToList
  where
    go (name, vhistory) =
        mapM_ go' $ mapToList vhistory
      where
        go' (version, time) = yield $ Uploaded name version time

-- FIXME put in conduit-combinators
parMapMC :: (MonadIO m, MonadBaseControl IO m)
         => Int
         -> (i -> m o)
         -> Conduit i m o
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
