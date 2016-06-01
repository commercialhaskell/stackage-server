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
  | shouldRedirect = redirect $ makeURL slug rest
  | final:_ <- reverse rest, ".html" `isSuffixOf` final = do
      render <- getUrlRender

      let stylesheet = render' $ StaticR haddock_style_css
          script = render' $ StaticR haddock_script_js
          bootstrap = render' $ StaticR haddock_bootstrap_css
          jquery = render' $ StaticR haddock_jquery_js
          render' = return . ContentText . render

          addExtra t@(EventEndElement "head") =
            [ EventBeginElement "link"
                [ ("rel", [ContentText "stylesheet"])
                , ("href", bootstrap)
                ]
            , EventEndElement "link"
            , EventBeginElement "link"
                 [ ("rel", [ContentText "stylesheet"])
                 , ("href", [ContentText "https://fonts.googleapis.com/css?family=Open+Sans"])
                 ]
            , EventEndElement "link"
            , EventBeginElement "link"
                 [ ("rel", [ContentText "stylesheet"])
                 , ("href", stylesheet)
                 ]
            , EventEndElement "link"
            , EventBeginElement "script"
                [ ("src", jquery)
                ]
            , EventEndElement "script"
            , EventBeginElement "script"
                [ ("src", script)
                ]
            , EventEndElement "script"
            , t
            ]
          addExtra t@(EventBeginElement "body" _) = [t] ++ nav
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

nav :: [Event]
nav =
  el "nav"
     [("class","navbar navbar-default")]
     (el "div"
         [("class","container")]
         (el "div"
             [("class","navbar-header")]
             (el "a"
                 [("href","https://haskell-lang.org/packages")
                 ,("class","navbar-brand")]
                 (el "span" [("class","logo")] [] ++ text "Haskell")) ++
          el "div"
             [("class","navbar-collapse")]
             (el "ul"
                 [("id","bootstrap-nav"),
                  ("class","nav navbar-nav")]
                 (concat [el "li" [] (el "a" [] (text "Source"))
                         ,el "li" [] (el "a" [] (text "Contents"))
                         ,el "li" [] (el "a" [] (text "Index"))
                         ]))))
  where text x = [EventContent (ContentText x)]
        el name props inner = open ++ inner ++ close
          where open =
                  [EventBeginElement name
                                     (map (\(k,v) -> (k,[ContentText v])) props)]
                close = [EventEndElement name]

getHaddockBackupR :: [Text] -> Handler ()
getHaddockBackupR rest = redirect $ concat
    $ "https://s3.amazonaws.com/haddock.stackage.org"
    : map (cons '/') rest
