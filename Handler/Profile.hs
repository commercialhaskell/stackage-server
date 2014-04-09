module Handler.Profile where

import Import
import Data.Slug (slugField)

userForm :: User -> Form User
userForm user = renderBootstrap $ User
    <$> areq slugField "User handle"
            { fsTooltip = Just "Used for URLs"
            } (Just $ userHandle user)
    <*> areq textField "Display name" (Just $ userDisplay user)
    <*> pure (userToken user)

getProfileR :: Handler Html
getProfileR = do
    Entity uid user <- requireAuth
    ((result, userWidget), enctype) <- runFormPost $ userForm user
    case result of
        FormSuccess user' -> do
            runDB $ replace uid user'
            setMessage "Profile updated"
            redirect ProfileR
        _ -> return ()
    emails <- runDB $ selectList [EmailUser ==. uid] [Asc EmailEmail]
    defaultLayout $ do
        setTitle "Your Profile"
        $(widgetFile "profile")

putProfileR :: Handler Html
putProfileR = getProfileR
