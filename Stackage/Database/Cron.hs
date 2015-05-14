module Stackage.Database.Cron
    ( stackageServerCron
    ) where

import ClassyPrelude.Conduit

stackageServerCron :: IO ()
stackageServerCron = error "FIXME: stackageServerCron not implemented"

{-

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
