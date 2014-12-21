module Handler.RefreshDeprecated where

import Import
import qualified Data.Aeson as Aeson
import Network.HTTP.Conduit (simpleHttp)
import Data.Hackage.DeprecationInfo

getRefreshDeprecatedR :: Handler Html
getRefreshDeprecatedR = do
  bs <- simpleHttp "http://hackage.haskell.org/packages/deprecated.json"
  case Aeson.decode bs of
    Nothing -> return "Failed to parse"
    Just info -> do
      runDB $ do
        deleteWhere ([] :: [Filter Deprecated])
        insertMany_ (deprecations info)
      runDB $ do
        deleteWhere ([] :: [Filter Suggested])
        insertMany_ (suggestions info)
      return "Done"
