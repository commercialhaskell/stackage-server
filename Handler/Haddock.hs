module Handler.Haddock
    ( getHaddockR
    , getHaddockBackupR
    ) where

import Import
import Stackage.Database
import Text.HTML.DOM (eventConduit)
import Text.XML (fromEvents)
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
            req <- parseUrl $ unpack $ makeURL slug rest
            (_, res) <- acquireResponse req >>= allocateAcquire
            doc <- responseBody res
                $$ eventConduit
                =$ concatMapC addExtra
                =$ mapC (Nothing, )
                =$ fromEvents
            sendResponse $ toHtml doc
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
getHaddockBackupR rest = track "Handler.Haddock.getHaddockBackupR" $  redirect $ concat
    $ "https://s3.amazonaws.com/haddock.stackage.org"
    : map (cons '/') rest
