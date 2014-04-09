module Handler.ResetToken where

import Import

postResetTokenR :: Handler ()
postResetTokenR = do
    Entity uid _ <- requireAuth
    runDB $ do
        token <- getToken
        update uid [UserToken =. token]
    setMessage "Token updated"
    redirect ProfileR
