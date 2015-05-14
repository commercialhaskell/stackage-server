module Stackage.Database.Cron
    ( stackageServerCron
    , loadFromS3
    , getHoogleDB
    ) where

import ClassyPrelude.Conduit
import Stackage.Database
import Network.HTTP.Client
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Filesystem (rename)
import Web.PathPieces (toPathPiece)
import Filesystem (isFile)
import Network.HTTP.Types (status200)

filename' :: Text
filename' = concat
    [ "stackage-database-"
    , tshow currentSchema
    , ".sqlite3"
    ]

keyName :: Text
keyName = "stackage-database/" ++ filename'

url :: Text
url = "https://s3.amazonaws.com/haddock.stackage.org/" ++ keyName

-- | Provides an action to be used to refresh the file from S3.
loadFromS3 :: IO (StackageDatabase, Manager -> IO ())
loadFromS3 = do
    let fp = fpFromText filename'
        fptmp = fp <.> "tmp"
    req <- parseUrl $ unpack url
    let download man = withResponse req man $ \res -> do
            runResourceT
                 $ bodyReaderSource (responseBody res)
                $$ sinkFile fptmp
            rename fptmp fp
    db <- openStackageDatabase fp
    return (db, download)

stackageServerCron :: IO ()
stackageServerCron = error "FIXME: stackageServerCron not implemented"


hoogleKey :: SnapName -> Text
hoogleKey name = concat
    [ "hoogle/"
    , toPathPiece name
    , "/"
    , VERSION_hoogle
    , ".hoo"
    ]

hoogleUrl :: SnapName -> Text
hoogleUrl n = "https://s3.amazonaws.com/haddock.stackage.org/" ++ hoogleKey n

getHoogleDB :: Manager -> SnapName -> IO (Maybe FilePath)
getHoogleDB man name = do
    let fp = fpFromText $ hoogleKey name
        fptmp = fp <.> "tmp"
    exists <- isFile fp
    if exists
        then return $ Just fp
        else do
            req' <- parseUrl $ unpack $ hoogleUrl name
            let req = req' { checkStatus = \_ _ _ -> Nothing }
            withResponse req man $ \res -> if responseStatus res == status200
                then do
                    runResourceT $ bodyReaderSource (responseBody res)
                                $$ sinkFile fptmp
                    rename fptmp fp
                    return $ Just fp
                else do
                    mapM brRead res >>= print
                    return Nothing

{-
    createStackageDatabase dbfile

import           Data.Streaming.Network (bindPortTCP)

data CabalLoaderEnv = CabalLoaderEnv
    { cleSettings :: !(AppConfig DefaultEnv Extra)
    , cleManager :: !Manager
    }

instance HasHackageRoot CabalLoaderEnv where
    getHackageRoot = hackageRoot . appExtra . cleSettings
instance HasHttpManager CabalLoaderEnv where
    getHttpManager = cleManager

cabalLoaderMain :: IO ()
cabalLoaderMain = do
    -- Hacky approach instead of PID files
    void $ catchIO (bindPortTCP 17834 "127.0.0.1") $ \_ ->
        error $ "cabal loader process already running, exiting"

    error "cabalLoaderMain"
    {- FIXME
    conf <- fromArgs parseExtra
    dbconf <- getDbConf conf
    pool <- Database.Persist.createPoolConfig dbconf
    manager <- newManager
    bs <- loadBlobStore manager conf
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let forceUpdate = lookup "STACKAGE_FORCE_UPDATE" env == Just "1"
    flip runLoggingT logFunc $ appLoadCabalFiles
        True -- update database?
        forceUpdate
        CabalLoaderEnv
            { cleSettings = conf
            , cleBlobStore = bs
            , cleManager = manager
            }
        dbconf
        pool

    let foundation = App
            { settings = conf
            , getStatic = error "getStatic"
            , connPool = pool
            , httpManager = manager
            , persistConfig = dbconf
            , appLogger = error "appLogger"
            , genIO = error "genIO"
            , blobStore = bs
            , haddockRootDir = error "haddockRootDir"
            , appDocUnpacker = error "appDocUnpacker"
            , widgetCache = error "widgetCache"
            , websiteContent = error "websiteContent"
            }
    createHoogleDatabases
        bs
        (flip (Database.Persist.runPool dbconf) pool)
        putStrLn
        (yesodRender foundation (appRoot conf))
  where
    logFunc loc src level str
        | level > LevelDebug = S.hPutStr stdout $ fromLogStr $ defaultLogStr loc src level str
        | otherwise = return ()
        -}


-}
