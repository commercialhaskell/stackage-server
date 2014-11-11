module Handler.CompressorStatus where

import Import

getCompressorStatusR :: Handler Html
getCompressorStatusR = do
    status <- getYesod >>= readIORef . compressorStatus
    defaultLayout $ do
        setTitle "Compressor thread status"
        [whamlet|
            <div .container>
                <h1>Compressor thread status
                <p>#{status}
        |]
