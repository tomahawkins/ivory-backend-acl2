-- | A DSL for ACL2.
module Ivory.Compile.ACL2.ACL2
  ( Expr (..)
  ) where

import Ivory.Compile.ACL2.SExpr

data Expr
  = Defun   String [String] Expr
  | MutualRecursion [Expr]
  | Cons    Expr Expr
  | Car     Expr
  | Cdr     Expr
  | Add     Expr Expr
  | Sub     Expr Expr
  | Not     Expr
  | And     [Expr]
  | Or      [Expr]
  | Implies Expr Expr
  | Equal   Expr Expr
  | If      Expr Expr Expr
  | Let'    [(String, Expr)] Expr
  | Var     String
  | Lit     String
  | Gt      Expr Expr
  | Ge      Expr Expr
  | Lt      Expr Expr
  | Le      Expr Expr
  | Nil

instance Show Expr where show = show . sExpr

sExpr :: Expr -> SExpr
sExpr a = case a of
  Defun   name args body -> SA [SV "defun", SV name, SA $ map SV args, sExpr body]
  MutualRecursion a -> SA $ SV "mutual-recursion" : map sExpr a
  Cons    a b -> f2 "cons" a b
  Car     a   -> f1 "car" a
  Cdr     a   -> f1 "cdr" a
  Add     a b -> f2 "+" a b
  Sub     a b -> f2 "-" a b
  Not     a   -> f1 "not" a
  And     a   -> fn "and" a
  Or      a   -> fn "or"  a
  Implies a b -> f2 "implies" a b
  Equal   a b -> f2 "equal" a b
  If      a b c -> f3 "if" a b c
  Let'    a b   -> SA $ SV "let*" : SA [ SA [SV name, sExpr a] | (name, a) <- a ] : [sExpr b]
  Var     a -> SV a
  Lit     a -> SV a
  Gt      a b -> f2 ">"  a b
  Ge      a b -> f2 ">=" a b
  Lt      a b -> f2 "<"  a b
  Le      a b -> f2 "<=" a b
  Nil         -> SV "nil"
  where
  f1 name a     = SA [SV name, sExpr a]
  f2 name a b   = SA [SV name, sExpr a, sExpr b]
  f3 name a b c = SA [SV name, sExpr a, sExpr b, sExpr c]
  fn name a     = SA $ SV name : map sExpr a
  
