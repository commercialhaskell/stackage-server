module Handler.Progress where

import Import

getProgressR :: Int -> Handler Html
getProgressR key = do
    app <- getYesod
    m <- readIORef $ progressMap app
    case lookup key m of
        Nothing -> notFound
        Just (ProgressWorking text) -> defaultLayout $ do
            addHeader "Refresh" "1"
            setTitle "Working..."
            [whamlet|<p>#{text}|]
        Just (ProgressDone text url) -> do
            setMessage $ toHtml text
            redirect url
