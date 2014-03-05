-- | Recursive topological sort.
module Ivory.Compile.ACL2.RecTopoSort
  ( recTopoSort
  ) where

import Data.Function (on)
import Data.List (nub, sort, sortBy)

recTopoSort :: (Eq a, Ord a) => (a -> [a]) -> [a] -> [[a]]
recTopoSort callees funs = f [] funs
  where
  --f :: Eq a => [[a]] -> [a] -> [[a]]
  f sofar remaining
    | null remaining = sofar
    | otherwise      = f (sofar ++ [next]) $ filter (not . flip elem next) remaining
    where
    next = head $ sortBy (compare `on` length) $ map (allCallees callees) remaining

allCallees :: (Eq a, Ord a) => (a -> [a]) -> a -> [a]
allCallees callees a = f [a]
  where
  f sofar
    | sofar == next = sofar
    | otherwise     = f next
    where
    next = sort $ nub $ concatMap callees sofar

