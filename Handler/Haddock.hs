module Handler.Haddock
    ( getHaddockR
    , getHaddockBackupR
    ) where

import Import
import Stackage.Database
import Text.HTML.TagStream.Types (Token' (..))
import Text.HTML.TagStream.ByteString (tokenStream, showToken)

makeURL :: SnapName -> [Text] -> Text
makeURL slug rest = concat
    $ "https://s3.amazonaws.com/haddock.stackage.org/"
    : toPathPiece slug
    : map (cons '/') rest

getHaddockR :: SnapName -> [Text] -> Handler TypedContent
getHaddockR slug rest
  | final:_ <- reverse rest, ".html" `isSuffixOf` final = do
      render <- getUrlRender

      let stylesheet = encodeUtf8 $ render $ StaticR haddock_style_css
          script = encodeUtf8 $ render $ StaticR haddock_script_js

          addExtra t@(TagClose "head") =
            [ TagOpen "link"
                [ ("rel", "stylesheet")
                , ("href", stylesheet)
                ]
                False
            , TagOpen "script"
                [ ("src", script)
                ]
                False
            , TagClose "script"
            , t
            ]
          addExtra t = [t]

      req <- parseUrl $ unpack $ makeURL slug rest
      (_, res) <- acquireResponse req >>= allocateAcquire

      respondSource typeHtml
         $ responseBody res
        $= tokenStream
        $= concatMapC addExtra
        $= mapC (Chunk . showToken id)
  | otherwise = redirect $ makeURL slug rest

getHaddockBackupR :: [Text] -> Handler ()
getHaddockBackupR rest = redirect $ concat
    $ "https://s3.amazonaws.com/haddock.stackage.org"
    : map (cons '/') rest
