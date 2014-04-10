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

loadCabalFiles :: ( MonadActive m
                  , MonadBaseControl IO m
                  , MonadThrow m
                  , MonadIO m
                  , MonadReader env m
                  , HasHttpManager env
                  , HasBlobStore env StoreKey
                  , HasHackageRoot env
                  , MonadLogger m
                  )
               => m ()
loadCabalFiles = do
    HackageRoot root <- liftM getHackageRoot ask
    $logDebug $ "Entering loadCabalFiles, root == " ++ root
    req <- parseUrl $ unpack $ root ++ "/00-index.tar.gz"
    withResponse req $ \res -> do
        $logDebug $ "Got a response, processing"
        bss <- lazyConsume $ responseBody res $= ungzip
        loop $ Tar.read $ fromChunks bss
  where
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
            _ -> return ()

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
            exists <- withResponse req $ \res ->
                if responseStatus res == status200
                    then do
                        responseBody res $$ storeWrite key
                        return True
                    else return False
            if exists
                then storeRead key
                else return Nothing

-- FIXME orphan
instance MonadActive m => MonadActive (LoggingT m) where
    monadActive = lift monadActive
