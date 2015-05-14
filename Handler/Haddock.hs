module Handler.Haddock
    ( getHaddockR
    ) where

import Import
import Stackage.Database

getHaddockR :: SnapName -> [Text] -> Handler ()
getHaddockR slug rest = redirect $ concat
    $ "http://haddock.stackage.org/"
    : toPathPiece slug
    : map (cons '/') rest
