module Mira.Intrinsics
  ( Intrinsic (..)
  , encodeIntrinsic
  , intrinsicACL2
  , allIntrinsics
  ) where

import Data.Maybe (fromJust)
import Data.List

import Mira.ACL2

data Intrinsic
  = Eq
  | Neq
  | Cond
  | Gt
  | Ge
  | Lt
  | Le
  | Not
  | And
  | Or
  | Mul
  | Mod
  | Add
  | Sub
  | Negate
  | Abs
  | Signum
  deriving (Show, Eq)

allIntrinsics :: [Intrinsic]
allIntrinsics =
  [ Eq
  , Neq
  , Cond
  , Gt
  , Ge
  , Lt
  , Le
  , Not
  , And
  , Or
  , Mul
  , Mod
  , Add
  , Sub
  , Negate
  , Abs
  , Signum
  ]

encodeIntrinsic :: Intrinsic -> Int
encodeIntrinsic a = fromJust $ elemIndex a allIntrinsics

intrinsicACL2 :: Intrinsic -> (Int -> Expr) -> Expr
intrinsicACL2 a arg = case a of
  Eq     -> if' (equal (arg 0) (arg 1)) 1 0
  Neq    -> if' (not' (equal (arg 0) (arg 1))) 1 0
  Cond   -> if' (zip' (arg 0)) (arg 2) (arg 1)
  Gt     -> if' (call ">"  [arg 0, arg 1]) 1 0
  Ge     -> if' (call ">=" [arg 0, arg 1]) 1 0
  Lt     -> if' (call "<"  [arg 0, arg 1]) 1 0
  Le     -> if' (call "<=" [arg 0, arg 1]) 1 0
  Not    -> if' (zip' (arg 0)) 1 0
  And    -> if' (or'  (zip' (arg 0)) (zip' (arg 1))) 0 1
  Or     -> if' (and' (zip' (arg 0)) (zip' (arg 1))) 0 1
  Mul    -> arg 0 * arg 1
  Mod    -> mod' (arg 0) (arg 1) 
  Add    -> arg 0 + arg 1
  Sub    -> arg 0 - arg 1
  Negate -> 0 - arg 1
  Abs    -> if' (call ">=" [arg 0, 0]) (arg 0) (0 - arg 0)
  Signum -> if' (call ">"  [arg 0, 0]) 1 $ if' (call "<" [arg 0, 0]) (-1) 0

