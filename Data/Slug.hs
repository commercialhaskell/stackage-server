module Data.Slug
    ( Slug
    , mkSlug
    , safeMakeSlug
    , unSlug
    , InvalidSlugException (..)
    , HasGenIO (..)
    , randomSlug
    , slugField
    ) where

import ClassyPrelude.Yesod
import Database.Persist.Sql (PersistFieldSql (sqlType))
import qualified System.Random.MWC as MWC
import Control.Monad.Reader (MonadReader, ask)
import GHC.Prim (RealWorld)
import Text.Blaze (ToMarkup)

newtype Slug = Slug { unSlug :: Text }
    deriving (Show, Read, Eq, Typeable, PersistField, ToMarkup)
instance PersistFieldSql Slug where
    sqlType = sqlType . liftM unSlug

mkSlug :: MonadThrow m => Text -> m Slug
mkSlug t
    | length t < minLen = throwM $ InvalidSlugException t "Too short"
    | length t > maxLen = throwM $ InvalidSlugException t "Too long"
    | any (not . validChar) t = throwM $ InvalidSlugException t "Contains invalid characters"
    | "-" `isPrefixOf` t = throwM $ InvalidSlugException t "Must not start with a hyphen"
    | otherwise = return $ Slug t
  where

minLen, maxLen :: Int
minLen = 3
maxLen = 30

validChar :: Char -> Bool
validChar c =
    ('A' <= c && c <= 'Z') ||
    ('a' <= c && c <= 'z') ||
    ('0' <= c && c <= '9') ||
    c == '-' ||
    c == '_'

data InvalidSlugException = InvalidSlugException !Text !Text
    deriving (Show, Typeable)
instance Exception InvalidSlugException

instance PathPiece Slug where
    toPathPiece = unSlug
    fromPathPiece = mkSlug

class HasGenIO a where
    getGenIO :: a -> MWC.GenIO
instance s ~ RealWorld => HasGenIO (MWC.Gen s) where
    getGenIO = id

safeMakeSlug :: (MonadIO m, MonadReader env m, HasGenIO env)
             => Text
             -> Bool -- ^ force some randomness?
             -> m Slug
safeMakeSlug orig forceRandom
    | needsRandom || forceRandom = do
        gen <- liftM getGenIO ask
        cs <- liftIO $ replicateM 3 $ MWC.uniformR (0, 61) gen
        return $ Slug $ cleaned ++ pack ('_':map toChar cs)
    | otherwise = return $ Slug cleaned
  where
    cleaned = take (maxLen - minLen - 1) $ dropWhile (== '-') $ filter validChar orig
    needsRandom = length cleaned < minLen

toChar :: Int -> Char
toChar i
    | i < 26    = toEnum $ fromEnum 'A' + i
    | i < 52    = toEnum $ fromEnum 'a' + i - 26
    | otherwise = toEnum $ fromEnum '0' + i - 52

randomSlug :: (MonadIO m, MonadReader env m, HasGenIO env)
           => Int -- ^ length
           -> m Slug
randomSlug (min maxLen . max minLen -> len) = do
    gen <- liftM getGenIO ask
    cs <- liftIO $ replicateM len $ MWC.uniformR (0, 61) gen
    return $ Slug $ pack $ map toChar cs

slugField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Slug
slugField =
    checkMMap go unSlug textField
  where
    go = return . either (Left . tshow) Right . mkSlug
