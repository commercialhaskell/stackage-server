{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Ensure that a function is only being run on a given input in one
-- thread at a time. All threads trying to make the call at once
-- return the same result.
module Control.SingleRun
    ( SingleRun
    , mkSingleRun
    , singleRun
    ) where

import RIO

-- | Captures all of the locking machinery and the function which is
-- run to generate results. Use 'mkSingleRun' to create this value.
data SingleRun k v = SingleRun
    { srVar :: MVar [(k, MVar (Res v))]
    -- ^ Keys and the variables containing their blocked
    -- computations. More ideal would be to use a Map, but we're
    -- avoiding dependencies outside of base in case this moves into
    -- auto-update.
    , srFunc :: forall m . MonadIO m => k -> m v
    }

-- | Create a 'SingleRun' value out of a function.
mkSingleRun :: MonadIO m => Eq k
            => (forall n . MonadIO n => k -> n v)
            -> m (SingleRun k v)
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
singleRun :: (MonadUnliftIO m, Eq k) => SingleRun k v -> k -> m v
singleRun sr@(SingleRun var f) k =
    -- Mask all exceptions so that we don't get killed between exiting
    -- the modifyMVar and entering the join, which could leave an
    -- empty MVar for a result that will never be filled.
    mask $ \restore ->
    join $ modifyMVar var $ \pairs ->
        case lookup k pairs of
            -- Another thread is already working on this, grab its result
            Just res -> do
                let action = restore $ do
                        res' <- readMVar res
                        case res' of
                            -- Other thread died by sync exception, rethrow
                            SyncException e -> throwIO e
                            -- Async exception, ignore and try again
                            AsyncException _ -> singleRun sr k
                            -- Success!
                            Success v -> return v
                -- Return unmodified pairs
                return (pairs, action)

            -- No other thread working
            Nothing -> do
                -- MVar we'll add to pairs to store the result and
                -- share with other threads
                resVar <- newEmptyMVar
                let action = do
                        -- Run the action and capture all exceptions
                        eres <- try $ restore $ f k

                        -- OK, we're done running, so let other
                        -- threads run this again.
                        modifyMVar_ var $ return . filter (\(k', _) -> k /= k')

                        case eres of
                            -- Exception occured. We'll rethrow it,
                            -- and store the exceptional result in the
                            -- result variable.
                            Left e -> do
                                putMVar resVar $ toRes e
                                throwIO e
                            -- Success! Store in the result variable
                            -- and return it
                            Right v -> do
                                putMVar resVar $ Success v
                                return v

                -- Modify pairs to include this variable.
                return ((k, resVar) : pairs, action)
