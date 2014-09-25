-- | Recursive topological sort.
module Ivory.Compile.ACL2.RecTopoSort
  ( recTopoSort
  , allDependencies
  ) where

import Data.Function (on)
import Data.List (nub, sort, sortBy)

-- | Topological sort of recursive functions.
recTopoSort :: (Eq a, Ord a) => (a -> [a]) -> [a] -> [[a]]
recTopoSort callees = filter (not . null) . f [] . sortBy (compare `on` length) . map (allDependencies callees)
  where
  f sofar a = case a of
    [] -> []
    a : b -> [ a | a <- a, not $ elem a sofar ] : f (nub $ a ++ sofar) b

-- | All the dependencies for a given functions, including that function.
allDependencies :: (Eq a, Ord a) => (a -> [a]) -> a -> [a]
allDependencies callees a = f [a]
  where
  f sofar
    | sofar == next = sofar
    | otherwise     = f next
    where
    next = sort $ nub $ sofar ++ concatMap callees sofar

