module Mira.Intrinsics
  ( Intrinsics (..)
  ) where

import Mira.ACL2 (Expr)

class Intrinsics i where
  intrinsicAdd    :: i
  intrinsicSub    :: i
  intrinsicLE     :: i
  intrinsicGE     :: i
  intrinsicImpl   :: i -> (Int -> Expr) -> Expr
  intrinsicEncode :: i -> Int

