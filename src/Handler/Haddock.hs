{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Haddock
    ( getHaddockR
    , getHaddockBackupR
    ) where

import Import
import qualified Data.Text as T (takeEnd)
import Stackage.Database

makeURL :: SnapName -> [Text] -> Text
makeURL snapName rest = concat
    $ "https://s3.amazonaws.com/"
    : haddockBucketName
    : "/"
    : toPathPiece snapName
    : map (cons '/') rest

shouldRedirect :: Bool
shouldRedirect = False

data DocType = DocHtml | DocJson

getHaddockR :: SnapName -> [Text] -> Handler TypedContent
getHaddockR snapName rest
    | shouldRedirect = do
        result <- redirectWithVersion snapName rest
        case result of
            Just route -> redirect route
            Nothing -> redirect $ makeURL snapName rest
    | Just docType <- mdocType = do
        result <- redirectWithVersion snapName rest
        case result of
            Just route -> redirect route
            Nothing -> do
                (contentType, plain) <-
                    case docType of
                        DocHtml -> do
                            mstyle <- lookupGetParam "style"
                            return ("text/html; charset=utf-8", mstyle /= Just "stackage")
                        DocJson ->
                            return ("application/jsontml; charset=utf-8", True)
                req <- parseRequest $ unpack $ makeURL snapName rest
                man <- getHttpManager <$> getYesod
                (_, res) <- runReaderT (acquireResponse req >>= allocateAcquire) man
                if plain
                    then respondSource contentType $ responseBody res .| mapC (Chunk . toBuilder)
                    else do
                        extra <- getExtra
                        respondSource contentType $
                            responseBody res .|
                            (do takeUntilChunk "</head>"
                                peekC >>= maybe (return ()) (const $ yield $ encodeUtf8 extra)
                                mapC id) .|
                            mapC (Chunk . toBuilder)
    | otherwise = redirect $ makeURL snapName rest
  where
    mdocType =
        case T.takeEnd 5 <$> headMay (reverse rest) of
            Just ".html" -> Just DocHtml
            Just ".json" -> Just DocJson
            _ -> Nothing
    getExtra = do
        render <- getUrlRender
        return $
            concat
                [ "<link rel='stylesheet' href='https://fonts.googleapis.com/css?family=Open+Sans'>"
                , "<link rel='stylesheet' href='"
                , render $ StaticR haddock_style_css
                , "'>"
                ]



takeUntilChunk :: Monad m => ByteString -> ConduitM ByteString ByteString m ()
takeUntilChunk fullNeedle =
    start
  where
    start = await >>= mapM_ start'

    start' bs =
      case checkNeedle fullNeedle bs of
        CNNotFound -> yield bs >> start
        CNFound before after -> yield before >> leftover after
        CNPartial before after newNeedle -> yield before >> loop (after:) newNeedle

    loop front needle =
        await >>= mapM_ loop'
      where
        loop' bs =
          if needle `isPrefixOf` bs
            then leftover $ concat $ front [bs]
            else
              case stripPrefix bs needle of
                Just needle' -> loop (front . (bs:)) needle'
                Nothing -> yieldMany (front [bs]) >> start

data CheckNeedle
    = CNNotFound
    | CNFound !ByteString
              !ByteString
    | CNPartial !ByteString
                !ByteString
                !ByteString

checkNeedle :: ByteString -> ByteString -> CheckNeedle
checkNeedle needle bs0 =
    loop 0
  where
    loop idx
      | idx >= length bs0 = CNNotFound
      | otherwise =
          case uncurry checkIndex $ splitAt idx bs0 of
            CNNotFound -> loop (idx + 1)
            res -> res

    checkIndex before bs
      | needle `isPrefixOf` bs = CNFound before bs
      | Just needle' <- stripPrefix bs needle = CNPartial before bs needle'
      | otherwise = CNNotFound

redirectWithVersion ::
       (GetStackageDatabase env m, MonadHandler m) => SnapName -> [Text] -> m (Maybe (Route App))
redirectWithVersion snapName rest =
    case rest of
        [pkg, file] | Just pname <- fromPathPiece pkg -> do
            mspi <- getSnapshotPackageInfo snapName pname
            case mspi of -- TODO: Should `Nothing` cause a 404 here, since haddock will fail?
                Nothing -> return Nothing -- error "That package is not part of this snapshot."
                Just spi -> do
                    return
                        (Just
                             (HaddockR
                                  snapName
                                  [toPathPiece $ PackageIdentifierP pname (spiVersion spi), file]))
        _ -> return Nothing

getHaddockBackupR :: [Text] -> Handler ()
getHaddockBackupR (snap':rest)
  | Just branch <- fromPathPiece snap' = track "Handler.Haddock.getHaddockBackupR" $ do
      snapName <- newestSnapshot branch >>= maybe notFound pure
      redirect $ HaddockR snapName rest
getHaddockBackupR rest = track "Handler.Haddock.getHaddockBackupR" $ redirect $ concat
    $ "https://s3.amazonaws.com/haddock.stackage.org"
    : map (cons '/') rest
