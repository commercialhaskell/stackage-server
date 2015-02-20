module Handler.UploadStackage where

import Import hiding (catch, get, update)
import qualified Import
import System.IO.Temp (withSystemTempFile, withSystemTempDirectory, openBinaryTempFile)
import Crypto.Hash.Conduit (sinkHash)
import Crypto.Hash (Digest, SHA1)
import Data.Byteable (toBytes)
import qualified Data.ByteString.Base16 as B16
import Data.Conduit.Zlib (gzip, ungzip)
import qualified Codec.Archive.Tar as Tar
import qualified Data.Text as T
import Filesystem.Path (splitExtension)
import Data.BlobStore
import Filesystem (createTree)
import Control.Monad.State.Strict (execStateT, get, put, modify)
import qualified Codec.Compression.GZip as GZip
import Control.Monad.Trans.Resource (allocate)
import System.Directory (removeFile, getTemporaryDirectory)
import System.Process (runProcess, waitForProcess)
import System.Exit (ExitCode (ExitSuccess))
import Data.Slug (mkSlug, SnapSlug (..), safeMakeSlug, unSlug)
import Control.Debounce

fileKey :: Text
fileKey = "stackage"

slugKey :: Text
slugKey = "slug"

getUploadStackageR :: Handler Html
getUploadStackageR = do
    _ <- requireAuth
    defaultLayout $ do
        setTitle "Upload"
        $(widgetFile "upload-stackage")

putUploadStackageR :: Handler TypedContent
putUploadStackageR = do
    uid <- requireAuthIdOrToken

    -- Only admin users can use slugs starting with "lts" and "nightly",
    -- enforce that here
    muser <- runDB $ Import.get uid
    extra <- getExtra
    let isAdmin =
            case muser of
                Nothing -> False
                Just user -> unSlug (userHandle user) `member` adminUsers extra
        allowedSlug Nothing = Nothing
        allowedSlug (Just t)
            | isAdmin = Just t
            | "lts" `isPrefixOf` t = Nothing
            | "nightly" `isPrefixOf` t = Nothing
            | otherwise = Just t

    mfile <- lookupFile fileKey
    mslug0 <- allowedSlug <$> lookupPostParam slugKey
    case mfile of
        Nothing -> invalidArgs ["Upload missing"]
        Just file -> do
            malias <- lookupPostParam "alias"
            mlts <- lookupPostParam "lts"
            mnightly <- lookupPostParam "nightly"

            tempDir <- liftIO getTemporaryDirectory
            (_releaseKey, (fp, handleOut)) <- allocate
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
            let initProgress = UploadProgress "Upload starting" Nothing
            key <- runDB $ insert initProgress

            -- We don't want to be writing progress updates to the database too
            -- frequently, so let's just do it once per second at most.
            -- Debounce to the rescue!
            statusRef <- newIORef initProgress
            writeToDB <- liftIO $ mkDebounce defaultDebounceSettings
                { debounceAction = do
                    up <- readIORef statusRef
                    runPool (persistConfig app) (replace key up) (connPool app)
                }

            let updateHelper :: MonadBase IO m => UploadProgress -> m ()
                updateHelper p = do
                    writeIORef statusRef p
                    liftBase writeToDB
                update :: MonadBase IO m => Text -> m ()
                update msg = updateHelper (UploadProgress msg Nothing)
                done msg route = do
                    render <- getUrlRender
                    updateHelper (UploadProgress msg $ Just $ render route)
                onExc e = done ("Exception occurred: " ++ tshow e) ProfileR
                setAlias = do
                    forM_ (malias >>= mkSlug) $ \alias -> do
                        deleteWhere [AliasUser ==. uid, AliasName ==. alias]
                        insert_ Alias
                            { aliasUser = uid
                            , aliasName = alias
                            , aliasTarget = ident
                            }
                whenAdmin = when isAdmin
                setLts sid = forM_ mlts
                    $ \lts -> whenAdmin
                    $ forM_ (parseLtsPair lts) $ \(major, minor) -> do
                        mx <- getBy $ UniqueLts major minor
                        when (isNothing mx) $ insert_ $ Lts major minor sid
                setNightly sid = forM_ mnightly $ \nightly -> whenAdmin $ do
                    now <- liftIO getCurrentTime
                    let day = utctDay now
                    mx <- getBy $ UniqueNightly day
                    when (isNothing mx) $ insert_ Nightly
                        { nightlyDay = day
                        , nightlyGhcVersion = nightly
                        , nightlyStackage = sid
                        }

            update "Starting"

            forkHandler onExc $ do
                now <- liftIO getCurrentTime
                baseSlug <- fmap SnapSlug $ mkSlug $ fromMaybe (tshow $ utctDay now) mslug0
                let initial = Stackage
                        { stackageUser = uid
                        , stackageIdent = ident
                        , stackageUploaded = now
                        , stackageTitle = "Untitled Stackage"
                        , stackageDesc = "No description provided"
                        , stackageHasHaddocks = False
                        , stackageSlug = baseSlug
                        }

                -- Evil lazy I/O thanks to tar package
                lbs <- readFile $ fpFromString fp
                withSystemTempDirectory "build00index." $ \dir -> do
                    LoopState _ stackage files _ contents cores <- execStateT (loop isAdmin update (Tar.read lbs)) LoopState
                        { lsRoot = fpFromString dir
                        , lsStackage = initial
                        , lsFiles = mempty
                        , lsIdent = ident
                        , lsContents = []
                        , lsCores = mempty
                        }
                    withSystemTempFile "newindex" $ \fp' h -> do
                        ec <- liftIO $ do
                            hClose h
                            let args = "cfz"
                                     : fp'
                                     : map fpToString (setToList files)
                            ph <- runProcess "tar" args (Just dir) Nothing Nothing Nothing Nothing
                            waitForProcess ph
                        if ec == ExitSuccess
                            then do
                                sourceFile (fpFromString fp') $$ storeWrite (CabalIndex ident)
                                sourceFile (fpFromString fp) $$ gzip =$ storeWrite (SnapshotBundle ident)
                                slug <- runDB $ do
                                    slug <- getUniqueSlug $ stackageSlug stackage
                                    sid <- insert stackage { stackageSlug = slug}
                                    forM_ contents $ \(name, version, overwrite) -> insert_ Package
                                        { packageStackage = sid
                                        , packageName' = name
                                        , packageVersion = version
                                        , packageOverwrite = overwrite
                                        , packageHasHaddocks = False
                                        , packageCore = Just $ name `member` cores
                                        }

                                    setAlias
                                    setLts sid
                                    setNightly sid

                                    return slug

                                done "Stackage created" $ SnapshotR slug StackageHomeR
                            else done "Error creating index file" ProfileR

            addHeader "X-Stackage-Ident" $ toPathPiece ident
            redirect $ ProgressR key
  where
    loop _ update Tar.Done = update "Finished processing files"
    loop _ _ (Tar.Fail e) = throwM e
    loop isAdmin update (Tar.Next entry entries) = do
        addEntry isAdmin update entry
        loop isAdmin update entries

    addEntry isAdmin update entry = do
        _ <- update $ "Processing file: " ++ pack (Tar.entryPath entry)
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
                    "slug" -> do
                        let t = decodeUtf8 $ toStrict lbs
                        when (isAdmin || not ("lts" `isPrefixOf` t || "nightly" `isPrefixOf` t)) $ do
                            slug <- safeMakeSlug t False
                            ls <- get
                            put ls { lsStackage = (lsStackage ls) { stackageSlug = SnapSlug slug } }
                    "hackage" -> forM_ (lines $ decodeUtf8 $ toStrict lbs) $ \line ->
                        case parseName line of
                            Just (name, version) -> do
                                $logDebug $ "hackage: " ++ tshow (name, version)
                                _ <- update $ concat
                                    [ "Adding Hackage package: "
                                    , toPathPiece name
                                    , "-"
                                    , toPathPiece version
                                    ]
                                msrc <- storeRead (HackageCabal name version)
                                case msrc of
                                    Nothing | name == "base" -> return () -- workaround in case base isn't uploaded to Hackage
                                    Nothing -> invalidArgs ["Unknown Hackage name/version: " ++ tshow (name, version)]
                                    Just src -> addFile False name version src

                            Nothing -> return ()

                    "core" -> forM_ (lines $ decodeUtf8 $ toStrict lbs) $ \name ->
                        modify $ \ls -> ls
                            { lsCores = insertSet (PackageName name)
                                      $ lsCores ls
                            }

                    fp | (base1, Just "gz") <- splitExtension fp
                       , (fpToText -> base, Just "tar") <- splitExtension base1 -> do
                            ident <- lsIdent <$> get
                            _ <- update $ concat
                                [ "Extracting cabal file for custom tarball: "
                                , base
                                ]
                            (name, version, cabalLBS) <- extractCabal lbs base
                            sourceLazy lbs $$ storeWrite (CustomSdist ident name version)
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
                put ls
                    { lsFiles = insertSet fp $ lsFiles ls
                    , lsContents
                        = (name, version, isOverride)
                        : lsContents ls
                    }
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

    , lsContents :: ![(PackageName, Version, IsOverride)] -- FIXME use SnocVector when ready
    , lsCores :: !(Set PackageName) -- ^ core packages
    }

type IsOverride = Bool

extractCabal :: (MonadLogger m, MonadThrow m)
             => LByteString
             -> Text -- ^ basename
             -> m (PackageName, Version, LByteString)
extractCabal lbs basename' =
    loop $ Tar.read $ GZip.decompress lbs
  where
    loop Tar.Done = error $ "extractCabal: cabal file missing for " ++ unpack basename'
    loop (Tar.Fail e) = throwM e
    loop (Tar.Next e es) = do
        $logDebug $ pack $ Tar.entryPath e
        case Tar.entryContent e of
            Tar.NormalFile lbs' _
                | Just (name, version) <- parseNameVersion (pack $ Tar.entryPath e)
               -> return (name, version, lbs')
            _ -> loop es

    parseNameVersion t = do
        [dir, filename'] <- Just $ T.splitOn "/" t
        let (name', version) = T.breakOnEnd "-" dir
        name <- stripSuffix "-" name'
        guard $ name ++ ".cabal" == filename'
        return (PackageName name, Version version)

-- | Get a unique version of the given slug by appending random numbers to the
-- end.
getUniqueSlug :: MonadIO m => SnapSlug -> ReaderT SqlBackend m SnapSlug
getUniqueSlug base =
    loop Nothing
  where
    loop msuffix = do
        slug <- checkSlug $ addSuffix msuffix
        ment <- getBy $ UniqueSnapshot slug
        case ment of
            Nothing -> return slug
            Just _ ->
                case msuffix of
                    Nothing -> loop $ Just (1 :: Int)
                    Just i
                        | i > 50 -> error "No unique slug found"
                        | otherwise -> loop $ Just $ i + 1

    txt = toPathPiece base

    addSuffix Nothing = txt
    addSuffix (Just i) = txt ++ pack ('-' : show i)

    checkSlug slug =
        case fromPathPiece slug of
            Nothing -> error $ "Invalid snapshot slug: " ++ unpack slug
            Just s -> return s
