module Stackage.Database.Cron
    ( stackageServerCron
    , newHoogleLocker
    , singleRun
    ) where

import ClassyPrelude.Conduit
import Stackage.PackageIndex.Conduit
import Database.Persist (Entity (Entity))
import qualified Codec.Archive.Tar as Tar
import Stackage.Database
import Network.HTTP.Client
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Filesystem (rename, removeTree, removeFile, isFile, createTree)
import Web.PathPieces (toPathPiece)
import Filesystem.Path.CurrentOS (parent, fromText, encodeString)
import Network.HTTP.Types (status200)
import           Data.Streaming.Network (bindPortTCP)
import           Network.AWS                   (Credentials (Discover), newEnv,
                                                send, chunkedFile, defaultChunkSize,
                                                envManager, runAWS)
import           Control.Monad.Trans.AWS       (trying, _Error)
import           Network.AWS.Data.Body         (toBody)
import           Network.AWS.S3                (ObjectCannedACL (OPublicRead),
                                                poACL, poContentType, putObject,
                                                BucketName(BucketName),
                                                ObjectKey(ObjectKey))
import Control.Lens (set, view)
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Zlib             (WindowBits (WindowBits),
                                                compress, ungzip)
import qualified Hoogle
import System.Directory (getAppUserDataDirectory)
import Control.SingleRun
import qualified Data.ByteString.Lazy as L
import System.FilePath (splitPath)
import System.Environment (getEnv)

hoogleKey :: SnapName -> Text
hoogleKey name = concat
    [ "hoogle/"
    , toPathPiece name
    , "/"
    , VERSION_hoogle
    , ".hoo"
    ]

hoogleUrl :: SnapName -> Text
hoogleUrl n = concat
    [ "https://s3.amazonaws.com/haddock.stackage.org/"
    , hoogleKey n
    ]

newHoogleLocker :: Bool -- ^ print exceptions?
                -> Manager
                -> IO (SingleRun SnapName (Maybe FilePath))
newHoogleLocker toPrint man = mkSingleRun $ \name -> do
    let fp = fromText $ hoogleKey name
        fptmp = encodeString fp <.> "tmp"

    exists <- isFile fp
    if exists
        then return $ Just (encodeString fp)
        else do
            req' <- parseRequest $ unpack $ hoogleUrl name
            let req = req' { decompress = const False }
            withResponse req man $ \res -> if responseStatus res == status200
                then do
                    createTree $ parent (fromString fptmp)
                    runConduitRes
                       $ bodyReaderSource (responseBody res)
                      .| ungzip
                      .| sinkFile fptmp
                    rename (fromString fptmp) fp
                    return $ Just $ encodeString fp
                else do
                    when toPrint $ mapM brRead res >>= print
                    return Nothing

stackageServerCron :: IO ()
stackageServerCron = do
    -- Hacky approach instead of PID files
    void $ catchIO (bindPortTCP 17834 "127.0.0.1") $ \_ ->
        error $ "cabal loader process already running, exiting"

    env <- newEnv Discover
    let upload :: FilePath -> ObjectKey -> IO ()
        upload fp key = do
            let fpgz = fp <.> "gz"
            runConduitRes
               $ sourceFile fp
              .| compress 9 (WindowBits 31)
              .| CB.sinkFile fpgz
            body <- chunkedFile defaultChunkSize fpgz
            let po =
                      set poACL (Just OPublicRead)
                   $  putObject "haddock.stackage.org" key body
            putStrLn $ "Uploading: " ++ tshow key
            eres <- runResourceT $ runAWS env $ trying _Error $ send po
            case eres of
                Left e -> error $ show (fp, key, e)
                Right _ -> putStrLn "Success"

    connstr <- getEnv "PGSTRING"

    let dbfp = PostgresConf
          { pgPoolSize = 5
          , pgConnStr = encodeUtf8 $ pack connstr
          }
    createStackageDatabase dbfp

    db <- openStackageDatabase dbfp

    do
        snapshots <- runReaderT snapshotsJSON db
        let key = ObjectKey "snapshots.json"
            po =
                  set poACL (Just OPublicRead)
               $  set poContentType (Just "application/json")
               $  putObject (BucketName "haddock.stackage.org") key (toBody snapshots)
        putStrLn $ "Uploading: " ++ tshow key
        eres <- runResourceT $ runAWS env $ trying _Error $ send po
        case eres of
            Left e -> error $ show (key, e)
            Right _ -> putStrLn "Success"

    names <- runReaderT (lastXLts5Nightly 50) db
    let manager = view envManager env

    locker <- newHoogleLocker False manager

    forM_ names $ \name -> do
        mfp <- singleRun locker name
        case mfp of
            Just _ -> putStrLn $ "Hoogle database exists for: " ++ toPathPiece name
            Nothing -> do
                mfp' <- createHoogleDB db manager name
                forM_ mfp' $ \fp -> do
                    let key = hoogleKey name
                    upload fp (ObjectKey key)
                    let dest = unpack key
                    createTree $ parent (fromString dest)
                    rename (fromString fp) (fromString dest)

createHoogleDB :: StackageDatabase -> Manager -> SnapName -> IO (Maybe FilePath)
createHoogleDB db man name = handleAny (\e -> print e $> Nothing) $ do
    putStrLn $ "Creating Hoogle DB for " ++ toPathPiece name
    req' <- parseRequest $ unpack tarUrl
    let req = req' { decompress = const True }

    unlessM (isFile (fromString tarFP)) $ withResponse req man $ \res -> do
        let tmp = tarFP <.> "tmp"
        createTree $ parent (fromString tmp)
        runConduitRes
           $ bodyReaderSource (responseBody res)
          .| sinkFile tmp
        rename (fromString tmp) (fromString tarFP)

    void $ tryIO $ removeTree (fromString bindir)
    void $ tryIO $ removeFile (fromString outname)
    createTree (fromString bindir)

    withSystemTempDirectory ("hoogle-" ++ unpack (toPathPiece name)) $ \tmpdir -> do
        allPackagePairs <- runConduitRes
            $ sourceTarFile False tarFP
           .| foldMapMC (liftIO . singleDB db name tmpdir)

        when (null allPackagePairs) $ error $ "No Hoogle .txt files found for " ++ unpack (toPathPiece name)

        stackDir <- getAppUserDataDirectory "stack"
        let indexTar = stackDir </> "indices" </> "Hackage" </> "00-index.tar"
        withBinaryFile indexTar ReadMode $ \h -> do
            let loop Tar.Done = return ()
                loop (Tar.Fail e) = throwIO e
                loop (Tar.Next e es) = go e >> loop es

                go e =
                    case (Tar.entryContent e, splitPath $ Tar.entryPath e) of
                        (Tar.NormalFile cabalLBS _, [pkg', ver', pkgcabal'])
                          | Just pkg <- stripSuffix "/" (pack pkg')
                          , Just ver <- stripSuffix "/" (pack ver')
                          , Just pkg2 <- stripSuffix ".cabal" (pack pkgcabal')
                          , pkg == pkg2
                          , lookup pkg allPackagePairs == Just ver ->
                                  runConduitRes
                                $ sourceLazy cabalLBS
                               .| sinkFile (tmpdir </> unpack pkg <.> "cabal")
                        _ -> return ()
            L.hGetContents h >>= loop . Tar.read

        let args =
                [ "generate"
                , "--database=" ++ outname
                , "--local=" ++ tmpdir
                ]
        putStrLn $ concat
            [ "Merging databases... ("
            , tshow args
            , ")"
            ]
        Hoogle.hoogle args

        putStrLn "Merge done"

        return $ Just outname
  where
    root = "hoogle-gen"
    bindir = root </> "bindir"
    outname = root </> "output.hoo"

    tarKey = toPathPiece name ++ "/hoogle/orig.tar"
    tarUrl = "https://s3.amazonaws.com/haddock.stackage.org/" ++ tarKey
    tarFP = root </> unpack tarKey

singleDB :: StackageDatabase
         -> SnapName
         -> FilePath -- ^ temp directory to write .txt files to
         -> Tar.Entry
         -> IO (Map Text Text)
singleDB db sname tmpdir e@(Tar.entryContent -> Tar.NormalFile lbs _) = do
    --putStrLn $ "Loading file for Hoogle: " ++ pack (Tar.entryPath e)

    let pkg = pack $ takeWhile (/= '.') $ Tar.entryPath e
    msp <- flip runReaderT db $ do
        Just (Entity sid _) <- lookupSnapshot sname
        lookupSnapshotPackage sid pkg
    case msp of
        Nothing -> do
            putStrLn $ "Unknown: " ++ pkg
            return mempty
        Just (Entity _ sp)  -> do
            let out = tmpdir </> unpack pkg <.> "txt"
                -- FIXME add @url directive
            runConduitRes $ sourceLazy lbs .| sinkFile out
            return $ singletonMap pkg (snapshotPackageVersion sp)
                {-
                docsUrl = concat
                    [ "https://www.stackage.org/haddock/"
                    , toPathPiece sname
                    , "/"
                    , pkgver
                    , "/index.html"
                    ] -}

singleDB _ _ _ _ = return mempty
