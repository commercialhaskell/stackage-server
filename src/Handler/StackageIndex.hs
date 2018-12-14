{-# LANGUAGE NoImplicitPrelude #-}
module Handler.StackageIndex where

import Import
import Stackage.Database.Types (haddockBucketName)

getStackageIndexR :: SnapName -> Handler TypedContent
getStackageIndexR slug =
    redirect $ concat
        [ "https://s3.amazonaws.com/"
        , haddockBucketName
        , "/package-index/"
        , toPathPiece slug
        , ".tar.gz"
        ]
