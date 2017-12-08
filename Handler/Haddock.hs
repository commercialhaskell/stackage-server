module Handler.Haddock
    ( getHaddockR
    , getHaddockBackupR
    ) where

import Import
import Stackage.Database
import Text.HTML.DOM (eventConduit)
import Text.XML.Stream.Render
import Data.XML.Types (Event (..), Content (..))

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
            let stylesheet = render' $ StaticR haddock_style_css
                render' = return . ContentText . render
                addExtra t@(EventEndElement "head") =
                  [ EventBeginElement "link"
                       [ ("rel", [ContentText "stylesheet"])
                       , ("href", [ContentText "https://fonts.googleapis.com/css?family=Open+Sans"])
                       ]
                  , EventEndElement "link"
                  , EventBeginElement "link"
                       [ ("rel", [ContentText "stylesheet"])
                       , ("href", stylesheet)
                       ]
                  , EventEndElement "link"
                  , t
                  ]
                addExtra t@(EventBeginElement "body" _) = [t]
                addExtra t = [t]
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
              _ -> respondSource "text/html; charset=utf-8"
                     $ responseBody res
                    .| eventConduit
                    .| concatMapC addExtra
                    .| renderBuilder def
                         { rsXMLDeclaration = False
                         }
                    .| mapC Chunk
  | otherwise = redirect $ makeURL slug rest

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
