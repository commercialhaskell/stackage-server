module Data.BlobStore
    ( BlobStore (..)
    , ToPath (..)
    , fileStore
    , HasBlobStore (..)
    , storeWrite
    , storeRead
    , storeExists
    , BackupToS3 (..)
    , cachedS3Store
    ) where

import ClassyPrelude.Yesod
import qualified Filesystem as F
import Control.Monad.Trans.Resource (release)
import qualified Aws
import Aws.S3 as Aws
import qualified System.IO as IO
import System.Directory (getTemporaryDirectory)

-- FIXME add a sendfile optimization
data BlobStore key = BlobStore
    { storeWrite'  :: !(forall m. MonadIO m => key -> Acquire (Sink ByteString m ()))
    , storeRead'   :: !(forall m. MonadIO m => key -> Acquire (Maybe (Source m ByteString)))
    , storeExists' :: !(forall m. MonadIO m => key -> m Bool)
    }

class HasBlobStore a key | a -> key where
    getBlobStore :: a -> BlobStore key
instance HasBlobStore (BlobStore key) key where
    getBlobStore = id

storeWrite :: (MonadResource m, MonadReader env m, HasBlobStore env key)
           => key
           -> Consumer ByteString m ()
storeWrite key = do
    store <- liftM getBlobStore ask
    (releaseKey, sink) <- allocateAcquire $ storeWrite' store key
    toConsumer sink
    release releaseKey

storeRead :: (MonadResource m, MonadReader env m, HasBlobStore env key)
          => key
          -> m (Maybe (Source m ByteString))
storeRead key = do
    store <- liftM getBlobStore ask
    (releaseKey, msrc) <- allocateAcquire $ storeRead' store key
    case msrc of
        Nothing -> do
            release releaseKey
            return Nothing
        Just src -> return $ Just $ src >> release releaseKey

storeExists :: (MonadIO m, MonadReader env m, HasBlobStore env key)
            => key
            -> m Bool
storeExists key = do
    store <- liftM getBlobStore ask
    storeExists' store key

class ToPath a where
    toPath :: a -> [Text]

fileStore :: ToPath key
          => FilePath -- ^ root
          -> BlobStore key
fileStore root = BlobStore
    { storeWrite' = \key -> (sinkHandle . snd) <$> mkAcquireType
        (do
            let fp = toFP root key
            F.createTree $ directory fp
            IO.openBinaryTempFile
                (fpToString $ directory fp)
                (fpToString $ filename fp))
        (\(fp, h) rt ->
            case rt of
                ReleaseException -> do
                    hClose h `finally` F.removeFile (fpFromString fp)
                _ -> do
                    hClose h
                    F.rename (fpFromString fp) (toFP root key))
    , storeRead' = \key -> (fmap sourceHandle) <$> mkAcquire
        ((Just <$> F.openFile (toFP root key) F.ReadMode)
            `catch` \e ->
                if isDoesNotExistError e
                    then return Nothing
                    else throwIO e)
        (maybe (return ()) hClose)
    , storeExists' = liftIO . F.isFile . toFP root
    }

toFP :: ToPath a => FilePath -> a -> FilePath
toFP root key = foldl' (\x y -> x </> fpFromText y) root (toPath key)

-- | Note: Only use with data which will never be modified!
cachedS3Store :: (BackupToS3 key, ToPath key)
              => FilePath -- ^ cache directory
              -> Aws.Credentials
              -> Text -- bucket FIXME Aws.Bucket
              -> Text -- ^ prefix within bucket
              -> Manager
              -> BlobStore key
cachedS3Store cache creds bucket prefix manager =
    self
  where
    self = BlobStore
        { storeWrite' = \key ->
            if shouldBackup key
                then do
                    tempDir <- liftIO getTemporaryDirectory
                    (fp, h) <- mkAcquire
                        (IO.openBinaryTempFile tempDir "store-write-cache")
                        (\(fp, h) -> hClose h >> F.removeFile (fpFromString fp))
                    return $ do
                        len <- getZipSink $ ZipSink (sinkHandle h) *> ZipSink lengthCE
                        liftIO $ hClose h
                        liftIO $ IO.withFile fp IO.ReadMode $ \inH -> runResourceT $ do
                            -- FIXME the need for this separate manager
                            -- indicates a serious bug in either aws or (more
                            -- likely) http-client, must investigate!
                            manager' <- newManager
                            res <- Aws.aws
                                (Aws.Configuration Aws.Timestamp creds
                                    $ Aws.defaultLog Aws.Error)
                                Aws.defServiceConfig
                                manager'
                                (Aws.putObject bucket (toS3Path key)
                                    $ requestBodySource len
                                    $ sourceHandle inH)
                            void $ Aws.readResponseIO res
                        liftIO $ IO.withFile fp IO.ReadMode $ \inH -> withAcquire
                            (storeWrite' (fileStore cache) key)
                            (sourceHandle inH $$)
                else storeWrite' (fileStore cache) key
        , storeRead' = \key ->
            if shouldBackup key
                then do
                    msrc <- storeRead' (fileStore cache) key
                    case msrc of
                        Just src -> return $ Just src
                        Nothing -> do
                            join $ liftIO $ handle (\S3Error{} -> return $ return Nothing) $ runResourceT $ do
                                res <- Aws.aws
                                    (Aws.Configuration Aws.Timestamp creds
                                        $ Aws.defaultLog Aws.Error)
                                    Aws.defServiceConfig
                                    manager
                                    (Aws.getObject bucket (toS3Path key))
                                gor <- Aws.readResponseIO res
                                let fp = toFP cache key
                                liftIO $ F.createTree $ directory fp
                                bracketOnError
                                    (liftIO $ IO.openBinaryTempFile
                                        (fpToString $ directory fp)
                                        (fpToString $ filename fp))
                                    (\(fpTmp, h) -> liftIO $ do
                                        hClose h
                                        F.removeFile (fpFromString fpTmp))
                                    $ \(fpTmp, h) -> do
                                        responseBody (Aws.gorResponse gor) $$+- sinkHandle h
                                        liftIO $ do
                                            hClose h
                                            F.rename (fpFromString fpTmp) fp
                                return $ storeRead' (fileStore cache) key -- FIXME optimize?
                else storeRead' (fileStore cache) key
        , storeExists' = \key ->
            if shouldBackup key
                then liftIO $ withAcquire (storeRead' self key)
                            $ \msrc -> return
                                $ maybe False (const True)
                                  (msrc :: Maybe (Source IO ByteString))
                else storeExists' (fileStore cache) key
        }

    toS3Path key = intercalate "/" $ filter (not . null) $ prefix : toPath key

class BackupToS3 key where
    shouldBackup :: key -> Bool
