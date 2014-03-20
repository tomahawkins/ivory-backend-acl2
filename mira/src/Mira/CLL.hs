-- | CLL: A C Like Language.
module Mira.CLL
  ( Proc    (..)
  , Stmt    (..)
  , Expr    (..)
  , Literal (..)
  , Var
  ) where

import Mira.CPS (Literal (..), Var)

data Proc i = Proc Var [Var] [Stmt i]

data Stmt i
  = Call   (Maybe Var) Var [Expr i]
  | If     (Expr i) [Stmt i] [Stmt i]
  | Return (Maybe (Expr i))
  | Assert (Expr i)
  | Assume (Expr i)
  | Let    Var (Expr i)
  | Loop   Var (Expr i) Bool (Expr i) [Stmt i]

data Expr i
  = Var       Var
  | Lit       Literal
  | Intrinsic i [Expr i]

