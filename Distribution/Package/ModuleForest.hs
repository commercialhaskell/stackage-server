-- Adopted from https://github.com/haskell/hackage-server/blob/master/Distribution/Server/Packages/ModuleForest.hs

module Distribution.Package.ModuleForest
    ( moduleName
    , moduleForest
    , ModuleTree(..)
    , ModuleForest
    , NameComponent
    ) where

import           Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import           Import

type NameComponent = Text

type ModuleForest = [ModuleTree]
data ModuleTree = Node { component  :: NameComponent
                       , isModule   :: Bool
                       , subModules :: ModuleForest
                       }
                deriving (Show, Eq)

moduleName :: Text -> ModuleName
moduleName = ModuleName.fromString . unpack

moduleForest :: [ModuleName] -> ModuleForest
moduleForest = foldr (addToForest . map pack . ModuleName.components) []

addToForest :: [NameComponent] -> ModuleForest -> ModuleForest
addToForest [] trees = trees
addToForest comps [] = mkSubTree comps
addToForest comps@(comp1:cs) (t@(component -> comp2):ts) = case
  compare comp1 comp2 of
    GT -> t : addToForest comps ts
    EQ -> Node comp2 (isModule t || null cs) (addToForest cs (subModules t)) : ts
    LT -> mkSubTree comps ++ t : ts

mkSubTree :: [Text] -> ModuleForest
mkSubTree []     = []
mkSubTree (c:cs) = [Node c (null cs) (mkSubTree cs)]
