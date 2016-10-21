-- | Ensure that a function is only being run on a given input in one
-- thread at a time. All threads trying to make the call at once
-- return the same result.
module Control.SingleRun
    ( SingleRun
    , mkSingleRun
    , singleRun
    ) where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (join)
import Prelude

-- | Captures all of the locking machinery and the function which is
-- run to generate results. Use 'mkSingleRun' to create this value.
data SingleRun k v = SingleRun
    { srVar :: MVar [(k, MVar (Res v))]
    , srFunc :: k -> IO v
    }

-- | Create a 'SingleRun' value out of a function.
mkSingleRun :: Eq k
            => (k -> IO v)
            -> IO (SingleRun k v)
mkSingleRun f = do
    var <- newMVar []
    return SingleRun
        { srVar = var
        , srFunc = f
        }

data Res v = SyncException SomeException
           | AsyncException SomeException
           | Success v

toRes :: SomeException -> Res v
toRes se =
    case fromException se of
        Just (SomeAsyncException _) -> AsyncException se
        Nothing -> SyncException se

-- | Get the result for the given input. If any other thread is
-- currently running this same computation, our thread will block on
-- that thread's result and then return it.
--
-- In the case that the other thread dies from a synchronous
-- exception, we will rethrow that same synchronous exception. If,
-- however, that other thread dies from an asynchronous exception, we
-- will retry.
singleRun :: Eq k => SingleRun k v -> k -> IO v
singleRun sr@(SingleRun var f) k =
    mask $ \restore ->
    join $ modifyMVar var $ \pairs ->
        case lookup k pairs of
            Just res -> do
                let action = restore $ do
                        res' <- readMVar res
                        case res' of
                            SyncException e -> throwIO e
                            AsyncException _ -> singleRun sr k
                            Success v -> return v
                return (pairs, action)
            Nothing -> do
                resVar <- newEmptyMVar
                let action = do
                        eres <- try $ restore $ f k
                        modifyMVar_ var $ return . filter (\(k', _) -> k /= k')
                        case eres of
                            Left e -> do
                                putMVar resVar $ toRes e
                                throwIO e
                            Right v -> do
                                putMVar resVar $ Success v
                                return v
                return ((k, resVar) : pairs, action)
