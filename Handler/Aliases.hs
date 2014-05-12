module Handler.Aliases where

import Import
import Data.Text (strip)

putAliasesR :: Handler ()
putAliasesR = do
    uid <- requireAuthId
    aliasesText <- runInputPost $ ireq textField "aliases"
    aliases <- mapM (parseAlias uid) $ lines aliasesText
    runDB $ do
        deleteWhere [AliasUser ==. uid]
        mapM_ insert_ aliases
    setMessage "Aliases updated"
    redirect ProfileR

parseAlias :: UserId -> Text -> Handler Alias
parseAlias uid t = maybe (invalidArgs ["Invalid alias: " ++ t]) return $ do
    name <- fromPathPiece x
    setid <- fromPathPiece y
    return $ Alias uid name setid
  where
    (strip -> x, (strip . drop 1) -> y) = break (== ':') t
