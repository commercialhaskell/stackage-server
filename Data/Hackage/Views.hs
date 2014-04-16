module Data.Hackage.Views where

import ClassyPrelude.Yesod
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version (anyVersion, intersectVersionRanges, earlierVersion, Version (..), simplifyVersionRange, VersionRange (..))
import Distribution.Text (simpleParse)
import Types hiding (Version (..))
import qualified Types
import Model
import Data.NonNull (fromNullable) -- FIXME expose from ClassyPrelude
import Data.Hackage (UploadHistory)

viewUnchanged :: Monad m
              => packageName -> version -> time
              -> GenericPackageDescription
              -> m GenericPackageDescription
viewUnchanged _ _ _ = return

helper :: Monad m
       => (Dependency -> m Dependency)
       -> GenericPackageDescription
       -> m GenericPackageDescription
helper f0 gpd = do
    a <- mapM (go f0) $ condLibrary gpd
    b <- mapM (go2 f0) $ condExecutables gpd
    c <- mapM (go2 f0) $ condTestSuites gpd
    d <- mapM (go2 f0) $ condBenchmarks gpd
    return gpd
        { condLibrary = a
        , condExecutables = b
        , condTestSuites = c
        , condBenchmarks = d
        }
  where
    go2 f (x, y) = do
        y' <- go f y
        return (x, y')

    go :: Monad m
       => (Dependency -> m Dependency)
       -> CondTree ConfVar [Dependency] a
       -> m (CondTree ConfVar [Dependency] a)
    go f (CondNode a constraints comps) = do
        constraints' <- mapM f constraints
        comps' <- mapM (goComp f) comps
        return $ CondNode a constraints' comps'

    goComp :: Monad m
           => (Dependency -> m Dependency)
           -> (condition, CondTree ConfVar [Dependency] a, Maybe (CondTree ConfVar [Dependency] a))
           -> m (condition, CondTree ConfVar [Dependency] a, Maybe (CondTree ConfVar [Dependency] a))
    goComp f (condition, tree, mtree) = do
        tree' <- go f tree
        mtree' <- mapM (go f) mtree
        return (condition, tree', mtree')

viewNoBounds :: Monad m
             => packageName -> version -> time
             -> GenericPackageDescription
             -> m GenericPackageDescription
viewNoBounds _ _ _ =
    helper go
  where
    go (Dependency name _range) = return $ Dependency name anyVersion

getAvailable name maxUploaded =
    map fst . filter ((<= maxUploaded) . snd) . mapToList . fromMaybe mempty . lookup name

viewPVP :: Monad m
        => UploadHistory
        -> packageName -> version -> UTCTime
        -> GenericPackageDescription
        -> m GenericPackageDescription
viewPVP uploadHistory _ _ uploaded =
    helper go
  where
    wiredIn = asSet $ setFromList $ words "base ghc template-haskell"

    toStr (Distribution.Package.PackageName name) = name

    go (Dependency name _) | toStr name `member` wiredIn = return $ Dependency name anyVersion
    go orig@(Dependency _ range) | hasUpperBound range = return orig
    go orig@(Dependency nameO@(toStr -> name) range) = do
        let available = getAvailable (fromString name) uploaded uploadHistory
        case fromNullable $ mapMaybe (simpleParse . unpack . toPathPiece) available of
            Nothing -> return orig
            Just vs ->
                case pvpBump $ maximum vs of
                    Nothing -> return orig
                    Just v -> return
                         $ Dependency nameO
                         $ simplifyVersionRange
                         $ intersectVersionRanges range
                         $ earlierVersion v

    pvpBump (Version (x:y:_) _) = Just $ Version [x, y + 1] []
    pvpBump _ = Nothing

    hasUpperBound AnyVersion = False
    hasUpperBound ThisVersion{} = True
    hasUpperBound LaterVersion{} = False
    hasUpperBound EarlierVersion{} = True
    hasUpperBound WildcardVersion{} = True
    hasUpperBound (UnionVersionRanges x y) = hasUpperBound x && hasUpperBound y
    hasUpperBound (IntersectVersionRanges x y) = hasUpperBound x || hasUpperBound y
    hasUpperBound (VersionRangeParens x) = hasUpperBound x
