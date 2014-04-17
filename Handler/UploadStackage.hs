module Handler.UploadStackage where

import Import hiding (catch, get)
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory, openBinaryTempFile)
import Crypto.Hash.Conduit (sinkHash)
import Control.Monad.Catch (MonadCatch (..))
import Crypto.Hash (Digest, SHA1)
import Data.Byteable (toBytes)
import qualified Data.ByteString.Base16 as B16
import Yesod.Core.Types (HandlerT (HandlerT), unHandlerT)
import Data.Conduit.Zlib (ungzip)
import qualified Codec.Archive.Tar as Tar
import qualified Data.Text as T
import Filesystem.Path (splitExtension)
import Data.BlobStore
import Filesystem (createTree)
import Control.Monad.State.Strict (execStateT, get, put)
import qualified Codec.Compression.GZip as GZip
import Control.Monad.Trans.Resource (unprotect, allocate)
import System.Directory (removeFile, getTemporaryDirectory)

fileKey :: Text
fileKey = "stackage"

getUploadStackageR :: Handler Html
getUploadStackageR = do
    _ <- requireAuth
    defaultLayout $ do
        setTitle "Upload"
        $(widgetFile "upload-stackage")

putUploadStackageR :: Handler TypedContent
putUploadStackageR = do
    uid <- requireAuthId
    mfile <- lookupFile fileKey
    case mfile of
        Nothing -> invalidArgs ["Upload missing"]
        Just file -> do
            tempDir <- liftIO getTemporaryDirectory
            (releaseKey, (fp, handleOut)) <- allocate
                (openBinaryTempFile tempDir "upload-stackage.")
                (\(fp, h) -> hClose h `finally` removeFile fp)
            digest <- fileSource file
                   $$ getZipSink (ZipSink sinkHash <* ZipSink (ungzip =$ sinkHandle handleOut))
            liftIO $ hClose handleOut

            let bs = toBytes (digest :: Digest SHA1)
                ident = PackageSetIdent $ decodeUtf8 $ B16.encode bs

            -- Check for duplicates
            mstackage <- runDB $ getBy $ UniqueStackage ident
            when (isJust mstackage) $ invalidArgs ["Stackage already exists"]

            app <- getYesod
            key <- atomicModifyIORef (nextProgressKey app) $ \i -> (i + 1, i + 1)
            let updateHelper :: MonadBase IO m => Progress -> m ()
                updateHelper p = atomicModifyIORef (progressMap app) $ \m -> (insertMap key p m, ())
                update :: MonadBase IO m => Text -> m ()
                update msg = updateHelper (ProgressWorking msg)
                done msg url = updateHelper (ProgressDone msg url)
                onExc e = done ("Exception occurred: " ++ tshow e) ProfileR

            update "Starting"

            forkHandler onExc $ do
                now <- liftIO getCurrentTime
                let initial = Stackage
                        { stackageUser = uid
                        , stackageIdent = ident
                        , stackageUploaded = now
                        , stackageTitle = "Untitled Stackage"
                        , stackageDesc = "No description provided"
                        }

                -- Evil lazy I/O thanks to tar package
                lbs <- readFile $ fpFromString fp
                withSystemTempDirectory "build00index." $ \dir -> do
                    LoopState _ stackage files _ <- execStateT (loop update (Tar.read lbs)) LoopState
                        { lsRoot = fpFromString dir
                        , lsStackage = initial
                        , lsFiles = mempty
                        , lsIdent = ident
                        }
                    entries <- liftIO $ Tar.pack dir $ map fpToString $ setToList files
                    let indexLBS = GZip.compress $ Tar.write entries
                    sourceLazy indexLBS $$ storeWrite (CabalIndex ident)
                    runDB $ insert stackage

                    done "Stackage created" $ StackageHomeR ident

            redirect $ ProgressR key
  where
    loop _ Tar.Done = return ()
    loop _ (Tar.Fail e) = throwM e
    loop update (Tar.Next entry entries) = do
        addEntry update entry
        loop update entries

    addEntry update entry = do
        update $ "Processing file: " ++ pack (Tar.entryPath entry)
        case Tar.entryContent entry of
            Tar.NormalFile lbs _ ->
                case filename $ fpFromString $ Tar.entryPath entry of
                    "desc" -> do
                        $logDebug $ "desc: " ++ tshow lbs
                        let (title, drop 1 -> desc) = break (== '\n')
                                                    $ decodeUtf8
                                                    $ toStrict lbs
                        ls <- get
                        put ls
                            { lsStackage = (lsStackage ls)
                                { stackageTitle = title
                                , stackageDesc = desc
                                }
                            }
                    "hackage" -> forM_ (lines $ decodeUtf8 $ toStrict lbs) $ \line ->
                        case parseName line of
                            Just (name, version) -> do
                                $logDebug $ "hackage: " ++ tshow (name, version)
                                update $ concat
                                    [ "Adding Hackage package: "
                                    , toPathPiece name
                                    , "-"
                                    , toPathPiece version
                                    ]
                                msrc <- storeRead (HackageCabal name version)
                                case msrc of
                                    Nothing -> invalidArgs ["Unknown Hackage name/version: " ++ tshow (name, version)]
                                    Just src -> addFile False name version src

                            Nothing -> return ()
                    fp | (base1, Just "gz") <- splitExtension fp
                       , (base, Just "tar") <- splitExtension base1
                       , Just (name, version) <- parseName (fpToText base) -> do
                            ident <- lsIdent <$> get
                            sourceLazy lbs $$ storeWrite (CustomSdist ident name version)
                            cabalLBS <- extractCabal lbs name version
                            addFile True name version $ sourceLazy cabalLBS
                    _ -> return ()
            _ -> return ()
      where
        addFile isOverride name version src = do
            ls <- get
            when (isOverride || fp `notMember` lsFiles ls) $ do
                let fp' = lsRoot ls </> fp
                liftIO $ createTree $ directory fp'
                src $$ sinkFile fp'
                put ls { lsFiles = insertSet fp $ lsFiles ls }
          where
            fp = mkFP name version

        mkFP name version
              = fpFromText (toPathPiece name)
            </> fpFromText (toPathPiece version)
            </> fpFromText (concat
                                [ toPathPiece name
                                , "-"
                                , toPathPiece version
                                , ".cabal"
                                ])

    parseName t =
        case T.breakOnEnd "-" t of
            ("", _) -> Nothing
            (_, "") -> Nothing
            (T.init -> name, version) -> Just (PackageName name, Version version)

data LoopState = LoopState
    { lsRoot :: !FilePath
    , lsStackage :: !Stackage
    , lsFiles :: !(Set FilePath)
    , lsIdent :: !PackageSetIdent
    }

extractCabal lbs name version =
    loop $ Tar.read $ GZip.decompress lbs
  where
    loop Tar.Done = error $ "extractCabal: cabal file missing for " ++ show (name, version)
    loop (Tar.Fail e) = throwM e
    loop (Tar.Next e es) = do
        $logDebug $ tshow (Tar.entryPath e, fp)
        case Tar.entryContent e of
            Tar.NormalFile lbs _ | Tar.entryPath e == fp -> return lbs
            _ -> loop es

    fp = unpack $ concat
        [ toPathPiece name
        , "-"
        , toPathPiece version
        , "/"
        , toPathPiece name
        , ".cabal"
        ]
