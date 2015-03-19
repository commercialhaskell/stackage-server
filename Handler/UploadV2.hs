module Handler.UploadV2
    ( putUploadV2R
    ) where

import Import
import Data.BlobStore
import Control.Concurrent.Lifted (threadDelay)
import Data.Slug (unSlug, mkSlug, SnapSlug (..))
import Control.Monad.Trans.Resource (allocate)
import System.Directory (removeFile, getTemporaryDirectory)
import System.IO.Temp (openBinaryTempFile, withSystemTempDirectory, withSystemTempFile)
import Crypto.Hash.Conduit (sinkHash)
import Crypto.Hash (Digest, SHA1)
import Data.Byteable (toBytes)
import qualified Data.ByteString.Base16 as B16
import System.Timeout.Lifted (timeout)
import Control.Concurrent.Async (async, cancel, waitCatchSTM)
import Yesod.Core.Types (HandlerT (..))
import Stackage.ServerBundle
import Stackage.BuildPlan
import Stackage.BuildConstraints
import Stackage.Prelude (display)
import Filesystem (createTree)
import Filesystem.Path (parent)
import Data.Conduit.Process
import Data.Yaml (decodeEither')

putUploadV2R :: Handler TypedContent
putUploadV2R = do
    uid <- requireAuthIdOrToken
    user <- runDB $ get404 uid
    extra <- getExtra
    when (unSlug (userHandle user) `notMember` adminUsers extra)
        $ permissionDenied "Only admins can upload V2 bundles"

    tempDir <- liftIO getTemporaryDirectory
    (_releaseKey, (bundleFP, bundleHOut)) <- allocate
        (openBinaryTempFile tempDir "upload.stackage2")
        (\(fp, h) -> hClose h `finally` removeFile fp)
    digest <- rawRequestBody $$ getZipSink
        (ZipSink (sinkHandle bundleHOut) *>
         ZipSink sinkHash)
    liftIO $ hClose bundleHOut

    let digestBS = toBytes (digest :: Digest SHA1)
        ident = PackageSetIdent $ decodeUtf8 $ B16.encode digestBS

    mstackage <- runDB $ getBy $ UniqueStackage ident
    when (isJust mstackage) $ invalidArgs ["Bundle already uploaded"]

    status <- liftIO $ newTVarIO ""

    let cont text = do
            sendChunkBS "CONT: "
            sendChunkText text
            sendChunkBS "\n"
            sendFlush

    -- Grab the internal HandlerT state to perform magic
    hd <- HandlerT return
    worker <- fmap snd $ flip allocate cancel $ async $ flip unHandlerT hd
            $ doUpload status uid ident (fpFromString bundleFP)

    respondSource "text/plain" $ do
        let displayStatus prev = do
                memsg <- liftIO $ timeout 20000000 $ atomically $ (do
                    msg <- readTVar status
                    checkSTM (msg /= prev)
                    return (Right msg)) <|> (Left <$> waitCatchSTM worker)
                case memsg of
                    Nothing -> do
                        cont "Still working"
                        displayStatus prev
                    Just (Left (Left e)) -> do
                        sendChunkText "FAILURE: "
                        sendChunkText $ tshow e
                        sendChunkText "\n"
                    Just (Left (Right t)) -> do
                        sendChunkText "SUCCESS: "
                        sendChunkText t
                        sendChunkText "\n"
                    Just (Right t) -> do
                        cont t
                        displayStatus t
        displayStatus ""

doUpload :: TVar Text
         -> UserId
         -> PackageSetIdent
         -> FilePath -- ^ temporary bundle file
         -> Handler Text
doUpload status uid ident bundleFP = do
    say $ "Uploading to persistent storage with ident " ++ toPathPiece ident
    sourceFile bundleFP $$ storeWrite (HaddockBundle ident)
    threadDelay 1000000 -- FIXME remove

    say $ "Unpacking bundle"

    (siType, siPlan, siDocMap :: DocMap) <-
      withSystemTempDirectory "uploadv2" $ \dir' -> do
        let dir = fpFromString dir'
        withCheckedProcess
            (proc "tar" ["xf", fpToString bundleFP])
                { cwd = Just dir'
                } $ \ClosedStream ClosedStream ClosedStream -> return ()

        let maxFileSize = 1024 * 1024 * 5
            yaml :: FromJSON a => FilePath -> Handler a
            yaml fp = do
                say $ "Parsing " ++ fpToText fp
                bs <- sourceFile (dir </> fp) $$ takeCE maxFileSize =$ foldC
                either throwM return $ decodeEither' bs

        (,,)
            <$> yaml "build-type.yaml"
            <*> yaml "build-plan.yaml"
            <*> yaml "docs-map.yaml"

    now <- liftIO getCurrentTime
    let day = tshow $ utctDay now

    let ghcVersion = display $ siGhcVersion $ bpSystemInfo siPlan
        slug' =
            case siType of
                STNightly -> "nightly-" ++ day
                STLTS major minor -> concat
                    [ "lts-"
                    , tshow major
                    , "."
                    , tshow minor
                    ]
        title =
            case siType of
                STNightly -> concat
                    [ "Stackage Nightly "
                    , day
                    , ", GHC "
                    , ghcVersion
                    ]
                STLTS major minor -> concat
                    [ "LTS Haskell "
                    , tshow major
                    , "."
                    , tshow minor
                    , ", GHC "
                    , ghcVersion
                    ]

    slug <- do
        slug2 <- mkSlug slug'
        when (slug' /= unSlug slug2) $ error $ "Slug not available: " ++ show slug'
        return $ SnapSlug slug2

    mexisting <- runDB $ getBy $ UniqueSnapshot slug
    route <- case mexisting of
        Just _ -> do
            say "Snapshot already exists"
            return $ SnapshotR slug StackageHomeR
        Nothing -> finishUpload
              title ident ghcVersion slug now siType siPlan siDocMap
              uid say
    render <- getUrlRender
    return $ render route
  where
    say = atomically . writeTVar status

finishUpload
    :: Text
    -> PackageSetIdent
    -> Text
    -> SnapSlug
    -> UTCTime
    -> SnapshotType
    -> BuildPlan
    -> Map Text PackageDocs
    -> UserId
    -> (Text -> Handler ())
    -> Handler (Route App)
finishUpload
  title ident ghcVersion slug now siType siPlan siDocMap
  uid say = do
    say "Creating index tarball"
    withSystemTempDirectory "buildindex.v2" $ \(fpFromString -> dir) -> do
        files <- forM (mapToList $ fmap ppVersion $ bpPackages siPlan) $ \(name', version') -> do
            let mpair = (,)
                    <$> fromPathPiece (display name')
                    <*> fromPathPiece (display version')
            (name, version) <-
                case mpair of
                    Nothing -> error $ "Could not parse: " ++ show (name', version')
                    Just pair -> return pair

            msrc <- storeRead (HackageCabal name version)
            src <-
                case msrc of
                    Nothing -> error $ "Cabal file not found for: " ++ show (name, version)
                    Just src -> return src

            let fp' =    fpFromText (toPathPiece name)
                     </> fpFromText (toPathPiece version)
                     </> fpFromText (concat
                            [ toPathPiece name
                            , "-"
                            , toPathPiece version
                            , ".cabal"
                            ])
            let fp = dir </> fp'

            liftIO $ createTree $ parent fp
            src $$ sinkFile fp
            return $ fpToString fp'

        withSystemTempFile "newindex.v2" $ \fp' h -> do
            liftIO $ do
                hClose h
                let args = "cfz"
                         : fp'
                         : files
                    cp = (proc "tar" args) { cwd = Just $ fpToString dir }
                withCheckedProcess cp $ \ClosedStream Inherited Inherited ->
                    return ()
            sourceFile (fpFromString fp') $$ storeWrite (CabalIndex ident)

    say $ "Attempting: " ++ tshow (slug, title)
    sid <- runDB $ do
        sid <- insert Stackage
            { stackageUser = uid
            , stackageIdent = ident
            , stackageSlug = slug
            , stackageUploaded = now
            , stackageTitle = title
            , stackageDesc = ""
            , stackageHasHaddocks = True
            }
        case siType of
            STNightly -> insert_ Nightly
                { nightlyDay = utctDay now
                , nightlyGhcVersion = ghcVersion
                , nightlyStackage = sid
                }
            STLTS major minor -> insert_ Lts
                { ltsMajor = major
                , ltsMinor = minor
                , ltsStackage = sid
                }

        let cores, nonCores :: Map PackageName Version
            cores = mapKeysWith const (PackageName . display)
                  $ fmap (Version . display)
                  $ siCorePackages
                  $ bpSystemInfo siPlan
            nonCores
                  = mapKeysWith const (PackageName . display)
                  $ fmap (Version . display . ppVersion)
                  $ bpPackages siPlan
        forM_ (mapToList $ cores ++ nonCores) $ \(name, version) -> do
            let PackageName nameT = name
            insert_ Package
                { packageStackage = sid
                , packageName' = name
                , packageVersion = version
                , packageHasHaddocks = nameT `member` siDocMap
                , packageOverwrite = False
                , packageCore = Just $ name `member` cores
                }
        return sid

    say $ concat
        [ "New snapshot with ID "
        , toPathPiece sid
        , " and slug "
        , toPathPiece slug
        , " created"
        ]

    render <- getUrlRender

    say "Updating docmap"
    runDB $ forM_ (mapToList siDocMap) $ \(package, PackageDocs version ms) -> do
        did <- insert Docs
            { docsName = PackageName package
            , docsVersion = Version version
            , docsUploaded = now
            , docsSnapshot = Just sid
            }
        forM_ (mapToList ms) $ \(name, pieces) -> do
            let url = render $ HaddockR slug pieces
            insert_ $ Module did name url

    return $ SnapshotR slug StackageHomeR
