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
          render' = return . ContentText . render

          addExtra t@(EventEndElement "head") =
            [ EventBeginElement "link"
                [ ("rel", [ContentText "stylesheet"])
                , ("href", stylesheet)
                ]
            , EventEndElement "link"
            , EventBeginElement "script"
                [ ("src", script)
                ]
            , EventEndElement "script"
            , t
            ]
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

getHaddockBackupR :: [Text] -> Handler ()
getHaddockBackupR rest = redirect $ concat
    $ "https://s3.amazonaws.com/haddock.stackage.org"
    : map (cons '/') rest
