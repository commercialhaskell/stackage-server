module Data.BlobStore
    ( BlobStore (..)
    , ToPath (..)
    , fileStore
    , HasBlobStore (..)
    , storeWrite
    , storeRead
    , storeExists
    ) where

import ClassyPrelude.Yesod
import qualified Filesystem as F
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans.Resource (release)
import qualified Aws

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
    { storeWrite' = \key -> sinkHandle <$> mkAcquire
        (do
            let fp = toFP key
            F.createTree $ directory fp
            F.openFile fp F.WriteMode)
        hClose
    , storeRead' = \key -> (fmap sourceHandle) <$> mkAcquire
        ((Just <$> F.openFile (toFP key) F.ReadMode)
            `catch` \e ->
                if isDoesNotExistError e
                    then return Nothing
                    else throwIO e)
        (maybe (return ()) hClose)
    , storeExists' = liftIO . F.isFile . toFP
    }
  where
    toFP key = foldl' (\x y -> x </> fpFromText y) root (toPath key)

{-
-- | Note: Only use with data which will never be modified!
cachedS3Store :: (BackupToS3 key, ToPath key)
              => FilePath -- ^ cache directory
              -> Aws.Bucket
              -> Text -- ^ prefix within bucket
              -> BlobStore key
cachedS3Store cache bucket prefix = BlobStore
    { storeWrite' = \key -> 
    }

class BackupToS3 key where
    shouldBackup :: key -> Bool
-}
