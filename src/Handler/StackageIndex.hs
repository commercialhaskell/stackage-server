{-# LANGUAGE NoImplicitPrelude #-}
module Handler.StackageIndex where

import Import

getStackageIndexR :: SnapName -> Handler TypedContent
getStackageIndexR slug = do
    bucketUrl <- getsYesod (appDownloadBucketUrl . appSettings)
    redirect $ concat
        [ bucketUrl
        , "/package-index/"
        , toPathPiece slug
        , ".tar.gz"
        ]
