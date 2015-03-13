module Handler.CompressorStatus where

import Import

getCompressorStatusR :: Handler Html
getCompressorStatusR = do
    status <- getYesod >>= liftIO . duGetStatus . appDocUnpacker
    defaultLayout $ do
        setTitle "Compressor thread status"
        [whamlet|
            <div .container>
                <h1>Compressor thread status
                <p>#{status}
        |]
