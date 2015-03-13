module Handler.Progress where

import Import

getProgressR :: UploadProgressId -> Handler Html
getProgressR key = do
    UploadProgress text mdest <- runDB $ get404 key
    case mdest of
        Nothing -> defaultLayout $ do
            addHeader "Refresh" "1"
            setTitle "Working..."
            [whamlet|<p>#{text}|]
        Just url -> do
            setMessage $ toHtml text
            redirect url
