module Handler.MirrorStatus
    ( getMirrorStatusR
    , mkUpdateMirrorStatus
    ) where

import Import
import Control.AutoUpdate
import Network.HTTP.Simple
import Data.Time (parseTimeM, diffUTCTime, addUTCTime)
import Text.XML.Stream.Parse
import Data.XML.Types (Event (EventContent), Content (ContentText))
import qualified Prelude

getMirrorStatusR :: Handler Html
getMirrorStatusR = do
    (status, widget) <- getYesod >>= liftIO . appMirrorStatus
    defaultLayout widget >>= sendResponseStatus status

mkUpdateMirrorStatus :: IO (IO (Status, Widget))
mkUpdateMirrorStatus = mkAutoUpdate defaultUpdateSettings
    { updateAction = go
    , updateFreq = 1000 * 1000 * 60 * 10 -- every 10 minutes
    }
  where
    go = do
        -- Ignore updates in the past hour, to give the mirrors a
        -- chance to process them.
        now <- getCurrentTime
        let oneHourAgo = addUTCTime (negate $ 60 * 60) now

        mhackageTime <- getHackageRecent oneHourAgo

        case mhackageTime of
            Nothing -> return (status500, "No Hackage time found, could just be a lot of recent uploads")
            Just hackageTime -> goHT hackageTime

    goHT hackageTime = do
        gitMods <- mapM (\(x, y, z) -> getLastModifiedGit x y z)
            [ ("commercialhaskell", "all-cabal-files", "current-hackage")
            , ("commercialhaskell", "all-cabal-hashes", "current-hackage")
            , ("commercialhaskell", "all-cabal-metadata", "master")
            ]
        tarballMods <- mapM getLastModifiedHTTP
            [ "http://hackage.fpcomplete.com/00-index.tar.gz"
            , "http://hackage.fpcomplete.com/01-index.tar.gz"
            ]
        otherMods <- mapM getLastModifiedHTTP
            [ "http://objects-us-west-1.dream.io/hackage-mirror/01-index.tar.gz"
            , "http://objects-us-west-1.dream.io/hackage-mirror/timestamp.json"
            ]
        let nonHackageMods = gitMods ++ tarballMods
            allMods = ("Hackage", hackageTime) : nonHackageMods ++ otherMods
            biggestDiff = Prelude.maximum $ map
                (\(_, other) -> diffUTCTime hackageTime other)
                nonHackageMods
            showLag x =
                case compare x 0 of
                    EQ -> ""
                    LT -> showDiff (abs x) ++ " (mirror newer)"
                    GT -> showDiff x ++ " (Hackage newer)"
            showDiff x =
                let (minutes', seconds) = floor x `divMod` (60 :: Int)
                    (hours, minutes) = minutes' `divMod` 60
                    showInt i
                      | i < 10 = '0' : show i
                      | otherwise = show i
                    showSuffix suffix i
                        | i == 0 = ""
                        | otherwise = showInt i ++ suffix
                 in unwords $ filter (not . null)
                        [ showSuffix "h" hours
                        , showSuffix "m" minutes
                        , showSuffix "s" seconds
                        ]
            widget = do
                setTitle "Mirror Status"
                [whamlet|
                    <h1>Mirror Status
                    <table border=1 cellpadding=1>
                        <tr>
                            <th>Name
                            <th>Last updated
                            <th>Lag
                        $forall (name, date) <- allMods
                            <tr>
                                <td>#{name}
                                <td>#{tshow date}
                                <td>#{showLag (diffUTCTime hackageTime date)}
                    $if biggestDiff > 0
                        <p>
                            Biggest lag: #{showLag biggestDiff}
                    $if isTooOld
                        <p style="color:red;font-size:300%">WARNING: Mirrors may be out of sync!
                |]
            isTooOld = biggestDiff > (60 * 60)
            status = if isTooOld then status500 else status200
        return (status, widget)

getLastModifiedHTTP :: Text -- ^ url
                    -> IO (Text, UTCTime)
getLastModifiedHTTP url = do
    req <- fmap (setRequestMethod "HEAD") $ parseUrlThrow $ unpack url
    res <- httpLBS req
    case getResponseHeader "last-modified" res of
        [x] -> do
            date <- parseTimeM
                True
                defaultTimeLocale
                "%a, %_d %b %Y %H:%M:%S %Z"
                (unpack $ decodeUtf8 x)
            return (url, date)
        x -> error $ "invalid last-modified for " ++ show (url, res, x)

getLastModifiedGit :: Text -- ^ org
                   -> Text -- ^ repo
                   -> Text -- ^ ref
                   -> IO (Text, UTCTime)
getLastModifiedGit org repo ref = do
    req <- parseUrlThrow $ unpack url
    res <- httpJSON $ addRequestHeader "User-Agent" "Stackage Server" req
    dateT <- lookupJ "commit" (getResponseBody res)
         >>= lookupJ "author"
         >>= lookupJ "date"
         >>= textJ
    date <- parseTimeM
        True
        defaultTimeLocale
        "%Y-%m-%dT%H:%M:%SZ"
        (unpack dateT)
    return (concat [org, "/", repo], date)
  where
    url = concat
        [ "https://api.github.com/repos/"
        , org
        , "/"
        , repo
        , "/commits/"
        , ref
        ]

lookupJ :: MonadThrow m => Text -> Value -> m Value
lookupJ key (Object o) =
    case lookup key o of
        Nothing -> error $ "Key not found: " ++ show key
        Just x -> return x
lookupJ key val = error $ concat
    [ "Looking up key "
    , show key
    , " on non-object "
    , show val
    ]

textJ :: MonadThrow m => Value -> m Text
textJ (String t) = return t
textJ v = error $ "Invalid value for textJ: " ++ show v

getHackageRecent :: UTCTime -- ^ latest time to continue
                 -> IO (Maybe UTCTime)
getHackageRecent latestTime =
    httpSink "https://hackage.haskell.org/packages/recent" sink
  where
    sink _ = parseBytes def
         =$= concatMapC getDate
         =$= filterC (<= latestTime)
         =$= headC

    getDate :: Event -> Maybe UTCTime
    getDate (EventContent (ContentText t)) = parseTimeM
        True
        defaultTimeLocale
        "%a %b %_d %H:%M:%S UTC %Y"
        (unpack t)
    getDate _ = Nothing
