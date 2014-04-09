module Handler.Email where

import Import
import Database.Persist.Sql (deleteWhereCount)

deleteEmailR :: EmailId -> Handler ()
deleteEmailR eid = do
    Entity uid _ <- requireAuth
    cnt <- runDB $ deleteWhereCount [EmailUser ==. uid, EmailId ==. eid]
    setMessage $
        if cnt > 0
            then "Email address deleted"
            else "No matching email address found"
    redirect ProfileR
