module Handler.Haddock
    ( getHaddockR
    , getHaddockBackupR
    ) where

import Import
import Stackage.Database

makeURL :: SnapName -> [Text] -> Text
makeURL slug rest = concat
    $ "https://s3.amazonaws.com/haddock.stackage.org/"
    : toPathPiece slug
    : map (cons '/') rest

shouldRedirect :: Bool
shouldRedirect = False

getHaddockR :: SnapName -> [Text] -> Handler TypedContent
getHaddockR slug rest
  | shouldRedirect = do
      result <- redirectWithVersion slug rest
      case result of
        Just route -> redirect route
        Nothing -> redirect $ makeURL slug rest
  | final:_ <- reverse rest, ".html" `isSuffixOf` final = do
      render <- getUrlRender
      result <- redirectWithVersion slug rest
      case result of
        Just route -> redirect route
        Nothing -> do
            let extra = concat
                  [ "<link rel='stylesheet' href='https://fonts.googleapis.com/css?family=Open+Sans'>"
                  , "<link rel='stylesheet' href='"
                  , render $ StaticR haddock_style_css
                  , "'>"
                  ]
            req <- parseRequest $ unpack $ makeURL slug rest
            (_, res) <- acquireResponse req >>= allocateAcquire
            mstyle' <- lookupGetParam "style"
            -- TODO: Uncomment line above. Restyling is really slow right now, still need to debug it.
            let mstyle =
                  case mstyle' of
                    Just "pretty" -> Nothing
                    _ -> Just ("plain" :: Text)
            case mstyle of
              Just "plain" -> respondSource "text/html; charset=utf-8"
                            $ responseBody res .| mapC (Chunk . toBuilder)
              _ -> respondSource "text/html; charset=utf-8" $ responseBody res .| (do
                    takeUntilChunk "</head>"
                    peekC >>= maybe (return ()) (const $ yield $ encodeUtf8 extra)
                    mapC id) .| mapC (Chunk . toBuilder)
  | otherwise = redirect $ makeURL slug rest

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

data CheckNeedle = CNNotFound | CNFound !ByteString !ByteString | CNPartial !ByteString !ByteString !ByteString

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

redirectWithVersion
  :: (GetStackageDatabase m,MonadHandler m,RedirectUrl (HandlerSite m) (Route App))
  => SnapName -> [Text] -> m (Maybe (Route App))
redirectWithVersion slug rest =
    case rest of
        [pkg,file] -> do
            Entity sid _ <- lookupSnapshot slug >>= maybe notFound return
            mversion <- getPackageVersionBySnapshot sid pkg
            case mversion of
                Nothing -> return Nothing -- error "That package is not part of this snapshot."
                Just version -> do
                    return (Just (HaddockR slug [pkg <> "-" <> version, file]))
        _ -> return Nothing

getHaddockBackupR :: [Text] -> Handler ()
getHaddockBackupR (snap':rest)
  | Just branch <- fromPathPiece snap' = track "Handler.Haddock.getHaddockBackupR" $ do
      snapName <- newestSnapshot branch >>= maybe notFound pure
      redirect $ HaddockR snapName rest
getHaddockBackupR rest = track "Handler.Haddock.getHaddockBackupR" $  redirect $ concat
    $ "https://s3.amazonaws.com/haddock.stackage.org"
    : map (cons '/') rest
