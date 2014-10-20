module Handler.Haddock where

import Import
import Data.BlobStore
import Filesystem (removeTree, isDirectory, createTree, isFile)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import System.IO.Temp (withSystemTempFile)
import Control.Exception (mask)
import System.Process (createProcess, proc, cwd, waitForProcess)
import System.Exit (ExitCode (ExitSuccess))
import Network.Mime (defaultMimeLookup)

form :: Form FileInfo
form = renderDivs $ areq fileField "tarball containing docs"
    { fsName = Just "tarball"
    } Nothing

getUploadHaddockR, putUploadHaddockR :: PackageSetIdent -> Handler Html
getUploadHaddockR ident = do
    uid <- requireAuthId
    Entity sid Stackage {..} <- runDB $ getBy404 $ UniqueStackage ident
    unless (uid == stackageUser) $ permissionDenied "You do not control this snapshot"
    ((res, widget), enctype) <- runFormPost form
    case res of
        FormSuccess fileInfo -> do
            fileSource fileInfo $$ storeWrite (HaddockBundle ident)
            runDB $ update sid [StackageHasHaddocks =. True]
            master <- getYesod
            getHaddockDir ident >>= liftIO . void . tryIO . removeTree
            setMessage "Haddocks uploaded"
            redirect $ StackageHomeR ident
        _ -> defaultLayout $ do
            setTitle "Upload Haddocks"
            $(widgetFile "upload-haddock")

putUploadHaddockR = getUploadHaddockR

getHaddockR :: PackageSetIdent -> [Text] -> Handler ()
getHaddockR ident rest = do
    mapM_ sanitize rest
    dir <- getHaddockDir ident
    master <- getYesod
    liftIO $ unlessM (isDirectory dir) $ haddockUnpacker master ident
    let fp = mconcat $ dir : map fpFromText rest

    whenM (liftIO $ isDirectory fp)
        $ redirect $ HaddockR ident $ rest ++ ["index.html"]
    unlessM (liftIO $ isFile fp) notFound

    let mime = defaultMimeLookup $ fpToText $ filename fp
    sendFile mime $ fpToString fp
  where
    sanitize p
        | ("/" `isInfixOf` p) || p `member` (asHashSet $ setFromList ["", ".", ".."]) =
            permissionDenied "Invalid request"
        | otherwise = return ()

createHaddockUnpacker :: FilePath -- ^ haddock root
                      -> BlobStore StoreKey
                      -> IO (PackageSetIdent -> IO ())
createHaddockUnpacker root store = do
    chan <- newChan

    mask $ \restore -> void $ forkIO $ forever $ do
        (ident, res) <- readChan chan
        try (restore $ go ident) >>= putMVar res
    return $ \ident -> do
        res <- newEmptyMVar
        writeChan chan (ident, res)
        takeMVar res >>= either (throwM . asSomeException) return
  where
    go ident = unlessM (isDirectory dir) $
               withSystemTempFile "haddock-bundle.tar.xz" $ \tempfp temph -> do
        withAcquire (storeRead' store (HaddockBundle ident)) $ \msrc ->
            case msrc of
                Nothing -> error "No haddocks exist for that snapshot"
                Just src -> src $$ sinkHandle temph
        hClose temph
        createTree dir
        (Nothing, Nothing, Nothing, ph) <- createProcess
            (proc "tar" ["xf", tempfp])
                { cwd = Just $ fpToString dir
                }
        ec <- waitForProcess ph
        if ec == ExitSuccess then return () else throwM ec
      where
        dir = root </> fpFromText (toPathPiece ident)
