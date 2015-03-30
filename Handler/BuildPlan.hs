{-# LANGUAGE ConstraintKinds #-}
module Handler.BuildPlan where

import Import hiding (get, PackageName (..), Version (..), DList)
import Data.Slug (SnapSlug)
import qualified Filesystem as F
import Data.Yaml (decodeFileEither)
import Control.Monad.State.Strict (get, modify, execStateT, MonadState)
import Control.Monad.Catch.Pure (runCatch)
import Stackage.Types
import Distribution.Package (PackageName (..))
import Data.Version (Version)

getBuildPlanR :: SnapSlug -> Handler Text
getBuildPlanR slug = do
    mlts <- runDB $ do
        Entity sid _ <- getBy404 $ UniqueSnapshot slug
        selectFirst [LtsStackage ==. sid] [Desc LtsMajor, Desc LtsMinor]
    Entity _ (Lts major minor _) <-
        case mlts of
            Just lts -> return lts
            Nothing -> invalidArgs ["Build plans are only available for LTS snapshots"]

    fp <- fmap fpToString $ ltsFP $ concat [tshow major, ".", tshow minor]
    bp <- liftIO $ decodeFileEither fp >>= either throwIO return
    -- treat packages as a set to skip duplicates and make order of parameters
    -- irrelevant
    packages <- setFromList <$> lookupGetParams "package"
    when (null packages) $ invalidArgs ["Must provide at least one package"]
    fullDeps <- (== Just "true") <$> lookupGetParam "full-deps"
    let eres = runCatch $ execStateT (getDeps bp fullDeps packages) (mempty, id)
    case eres of
        Left e -> invalidArgs [tshow e]
        Right (_, front) -> return $ unlines $ flip map (front [])
            $ \(x, y) -> unwords [display x, display y]

type HttpM env m =
    ( MonadReader env m
    , MonadIO m
    , HasHttpManager env
    , MonadBaseControl IO m
    , MonadThrow m
    )

ltsFP :: HttpM env m
      => Text
      -> m FilePath
ltsFP ltsVer = do
    --dir <- liftIO $ F.getAppDataDirectory "stackage-bootstrap"
    let dir = "/tmp/stackage-bootstrap" -- HOME not set on server
    let fp = dir </> fpFromText ("lts-" ++ ltsVer) <.> "yaml"
    exists <- liftIO $ F.isFile fp
    if exists
        then return fp
        else do
            liftIO $ F.createTree dir
            let tmp = fp <.> "tmp"
            download ltsVer tmp
            liftIO $ F.rename tmp fp
            return fp

download :: HttpM env m
         => Text
         -> FilePath
         -> m ()
download ltsVer dest = do
    req <- parseUrl $ unpack $ concat
    {-
        [ "https://raw.githubusercontent.com/fpco/lts-haskell/master/lts-"
        , ltsVer
        , ".yaml"
        ]
        -}
        [ "https://cdn.rawgit.com/fpco/lts-haskell/master/lts-"
        , ltsVer
        , "1.14.yaml"
        ]
    withResponse req $ \res -> liftIO $ F.withFile dest F.WriteMode $ \h ->
        responseBody res $$ sinkHandle h

type TheState =
    ( Set PackageName
    , DList (PackageName, Version)
    )
type DList a = [a] -> [a]

getDeps :: (MonadThrow m, MonadState TheState m)
        => BuildPlan
        -> Bool
        -> Set Text
        -> m ()
getDeps BuildPlan {..} fullDeps =
    mapM_ (goName . PackageName . unpack)
  where
    goName name = do
        (s, _) <- get
        when (name `notMember` s) $
            case lookup name bpPackages of
                Just pkg -> goPkg name pkg
                Nothing ->
                    case lookup name $ siCorePackages bpSystemInfo of
                        Just version -> do
                            addToSet name
                            addToList name version
                        Nothing -> throwM $ PackageNotFound name

    goPkg name PackagePlan {..} = do
        addToSet name
        forM_ (mapToList $ sdPackages ppDesc) $ \(name', depInfo) ->
            when (includeDep depInfo) (goName name')
        addToList name ppVersion

    addToSet name = modify $ \(s, front) -> (insertSet name s, front)

    addToList name version =
        modify $ \(s, front) -> (s, front . (x:))
      where
        x = (name, version)

    includeDep DepInfo {..} =
        fullDeps ||
        CompLibrary    `member` diComponents ||
        CompExecutable `member` diComponents

data PackageNotFound = PackageNotFound PackageName
    deriving (Show, Typeable)
instance Exception PackageNotFound
