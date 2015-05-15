module Stackage.Database.Cron
    ( stackageServerCron
    , loadFromS3
    , getHoogleDB
    ) where

import ClassyPrelude.Conduit
import Stackage.PackageIndex.Conduit
import Database.Persist (Entity (Entity))
import Data.Char (isAlpha)
import qualified Codec.Archive.Tar as Tar
import Stackage.Database
import Network.HTTP.Client
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Filesystem (rename, removeTree, removeFile)
import Web.PathPieces (toPathPiece)
import Filesystem (isFile, createTree)
import Filesystem.Path (parent)
import Control.Monad.State.Strict (StateT, get, put)
import Network.HTTP.Types (status200)
import           Data.Streaming.Network (bindPortTCP)
import           Network.AWS                   (Credentials (Discover),
                                                Region (NorthVirginia), getEnv,
                                                send, sourceFileIO, envManager )
import           Network.AWS.S3                (ObjectCannedACL (PublicRead),
                                                poACL,
                                                putObject)
import Control.Lens (set, view)
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Zlib             (WindowBits (WindowBits),
                                                compress)
import qualified Hoogle

filename' :: Text
filename' = concat
    [ "stackage-database-"
    , tshow currentSchema
    , ".sqlite3"
    ]

keyName :: Text
keyName = "stackage-database/" ++ filename'

url :: Text
url = concat
    [ "https://s3.amazonaws.com/haddock.stackage.org/"
    , keyName
    , ".gz"
    ]

-- | Provides an action to be used to refresh the file from S3.
loadFromS3 :: IO (StackageDatabase, Manager -> IO ())
loadFromS3 = do
    let fp = fpFromText keyName
        fptmp = fp <.> "tmp"
    req <- parseUrl $ unpack url
    let download man = withResponse req man $ \res -> do
            createTree $ parent fptmp
            runResourceT
                 $ bodyReaderSource (responseBody res)
                $$ sinkFile fptmp
            rename fptmp fp
    db <- openStackageDatabase fp
    return (db, download)

hoogleKey :: SnapName -> Text
hoogleKey name = concat
    [ "hoogle/"
    , toPathPiece name
    , "/"
    , VERSION_hoogle
    , ".hoo"
    ]

hoogleUrl :: SnapName -> Text
hoogleUrl n = concat
    [ "https://s3.amazonaws.com/haddock.stackage.org/"
    , hoogleKey n
    , ".gz"
    ]

getHoogleDB :: Bool -- ^ print exceptions?
            -> Manager -> SnapName -> IO (Maybe FilePath)
getHoogleDB toPrint man name = do
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
                    createTree $ parent fptmp
                    runResourceT $ bodyReaderSource (responseBody res)
                                $$ sinkFile fptmp
                    rename fptmp fp
                    return $ Just fp
                else do
                    when toPrint $ mapM brRead res >>= print
                    return Nothing

stackageServerCron :: IO ()
stackageServerCron = do
    -- Hacky approach instead of PID files
    void $ catchIO (bindPortTCP 17834 "127.0.0.1") $ \_ ->
        error $ "cabal loader process already running, exiting"

    env <- getEnv NorthVirginia Discover
    let upload :: FilePath -> Text -> IO ()
        upload fp key = do
            let fpgz = fpToString $ fp <.> "gz"
            runResourceT $ sourceFile fp
                        $$ compress 9 (WindowBits 31)
                        =$ CB.sinkFile fpgz
            body <- sourceFileIO fpgz
            let po = 
                      set poACL (Just PublicRead)
                   $  putObject body "haddock.stackage.org" key
            putStrLn $ "Uploading: " ++ key
            eres <- runResourceT $ send env po
            case eres of
                Left e -> error $ show (fp, key, e)
                Right _ -> putStrLn "Success"

    let dbfp = fpFromText keyName
    _ <- return (upload, dbfp)
    --createStackageDatabase dbfp
    --upload dbfp keyName

    (db, _) <- loadFromS3
    names <- runReaderT last5Lts5Nightly db
    let manager = view envManager env
    forM_ names $ \name -> do
        mfp <- getHoogleDB False manager name
        case mfp of
            Just _ -> putStrLn $ "Hoogle database exists for: " ++ toPathPiece name
            Nothing -> do
                mfp' <- createHoogleDB db manager name
                forM_ mfp' $ \fp -> do
                    let key = hoogleKey name
                    upload fp key
                    let dest = fpFromText key
                    createTree $ parent dest
                    rename fp dest

createHoogleDB :: StackageDatabase -> Manager -> SnapName -> IO (Maybe FilePath)
createHoogleDB db man name = handleAny (\e -> print e $> Nothing) $ do
    req' <- parseUrl $ unpack tarUrl
    let req = req' { decompress = const True }

    unlessM (isFile tarFP) $ withResponse req man $ \res -> do
        let tmp = tarFP <.> "tmp"
        createTree $ parent tmp
        runResourceT $ bodyReaderSource (responseBody res)
                    $$ sinkFile tmp
        rename tmp tarFP

    void $ tryIO $ removeTree bindir
    void $ tryIO $ removeFile outname
    createTree bindir

    dbs <- runResourceT
        $ sourceTarFile False (fpToString tarFP)
       $$ evalStateC 1 (mapMC (singleDB db name bindir))
       =$ sinkList

    putStrLn "Merging databases..."
    Hoogle.mergeDatabase (map fpToString $ catMaybes dbs) (fpToString outname)
    putStrLn "Merge done"

    return $ Just outname
  where
    root = "hoogle-gen"
    bindir = root </> "bindir"
    outname = root </> "output.hoo"

    tarKey = toPathPiece name ++ "/hoogle/orig.tar"
    tarUrl = "https://s3.amazonaws.com/haddock.stackage.org/" ++ tarKey
    tarFP = root </> fpFromText tarKey

singleDB :: StackageDatabase
         -> SnapName
         -> FilePath -- ^ bindir to write to
         -> Tar.Entry
         -> StateT Int (ResourceT IO) (Maybe FilePath)
singleDB db sname bindir e@(Tar.entryContent -> Tar.NormalFile lbs _) = do
    idx <- get
    put $! idx + 1
    putStrLn $ "Loading file for Hoogle: " ++ pack (Tar.entryPath e)

    let pkg = pack $ takeWhile (/= '.') $ Tar.entryPath e
    msp <- flip runReaderT db $ do
        Just (Entity sid _) <- lookupSnapshot sname
        lookupSnapshotPackage sid pkg
    case msp of
        Nothing -> do
            putStrLn $ "Unknown: " ++ pkg
            return Nothing
        Just (Entity _ sp) -> do
            let ver = snapshotPackageVersion sp
                pkgver = concat [pkg, "-", ver]
                out = bindir </> fpFromString (show idx) <.> "hoo"
                src' = unlines
                     $ haddockHacks (Just $ unpack docsUrl)
                     $ lines
                     $ unpack
                     $ decodeUtf8 lbs
                docsUrl = concat
                    [ "https://www.stackage.org/haddock/"
                    , toPathPiece sname
                    , "/"
                    , pkgver
                    , "/index.html"
                    ]

            _errs <- liftIO $ Hoogle.createDatabase "" Hoogle.Haskell [] src' $ fpToString out

            return $ Just out
singleDB _ _ _ _ = return Nothing

---------------------------------------------------------------------
-- HADDOCK HACKS
-- (Copied from hoogle-4.2.36/src/Recipe/Haddock.hs)
-- Modifications:
-- 1) Some name qualification
-- 2) Explicit type sig due to polymorphic elem
-- 3) Fixed an unused binding warning

-- Eliminate @version
-- Change :*: to (:*:), Haddock bug
-- Change !!Int to !Int, Haddock bug
-- Change instance [overlap ok] to instance, Haddock bug
-- Change instance [incoherent] to instance, Haddock bug
-- Change instance [safe] to instance, Haddock bug
-- Change !Int to Int, HSE bug
-- Drop {-# UNPACK #-}, Haddock bug
-- Drop everything after where, Haddock bug

haddockHacks :: Maybe Hoogle.URL -> [String] -> [String]
haddockHacks loc src = maybe id haddockPackageUrl loc (translate src)
    where
        translate :: [String] -> [String]
        translate = map (unwords . g . map f . words) . filter (not . isPrefixOf "@version ")

        f "::" = "::"
        f (':':xs) = "(:" ++ xs ++ ")"
        f ('!':'!':x:xs) | isAlpha x = xs
        f ('!':x:xs) | isAlpha x || x `elem` ("[(" :: String) = x:xs
        f x | x `elem` ["[overlap","ok]","[incoherent]","[safe]"] = ""
        f x | x `elem` ["{-#","UNPACK","#-}"] = ""
        f x = x

        g ("where":_) = []
        g (x:xs) = x : g xs
        g [] = []

haddockPackageUrl :: Hoogle.URL -> [String] -> [String]
haddockPackageUrl x = concatMap f
    where f y | "@package " `isPrefixOf` y = ["@url " ++ x, y]
              | otherwise = [y]
