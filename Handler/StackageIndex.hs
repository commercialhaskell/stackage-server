module Handler.StackageIndex where

import Import
import Data.BlobStore
import Stackage.Database

getStackageIndexR :: SnapName -> Handler TypedContent
getStackageIndexR slug = do
    -- Insecure, courtesy of cabal-install
    redirect $ concat
        [ "http://haddock.stackage.org/package-index/"
        , toPathPiece slug
        , ".tar.gz"
        ]
