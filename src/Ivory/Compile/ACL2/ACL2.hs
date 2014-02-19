-- | A DSL for ACL2.
module Ivory.Compile.ACL2.ACL2
  ( Expr
  , defun
  , call
  , cons
  , car
  , cdr
  , nth
  , let'
  , var
  , lit
  , nil
  , add
  ) where

import Ivory.Compile.ACL2.SExpr

data Expr
  = Defun   String [String] Expr
  | MutualRecursion [Expr]
  | Call    String [Expr]
  | Let'    [(String, Expr)] Expr
  | Var     String
  | Lit     String
  | Nil

instance Show Expr where show = show . sExpr

sExpr :: Expr -> SExpr
sExpr a = case a of
  Defun   name args body -> SA [SV "defun", SV name, SA $ map SV args, sExpr body]
  MutualRecursion a -> SA $ SV "mutual-recursion" : map sExpr a
  Call    a b -> SA $ SV a : map sExpr b
  Let'    a b   -> SA $ SV "let*" : SA [ SA [SV name, sExpr a] | (name, a) <- a ] : [sExpr b]
  Var     a -> SV a
  Lit     a -> SV a
  Nil         -> SV "nil"

defun :: String -> [String] -> Expr -> Expr
defun = Defun
 
call :: String -> [Expr] -> Expr
call = Call

cons :: Expr -> Expr -> Expr
cons a b = Call "cons" [a, b]

car :: Expr -> Expr
car a = Call "car" [a]

cdr :: Expr -> Expr
cdr a = Call "cdr" [a]

nth :: Expr -> Expr -> Expr
nth a b = Call "nth" [a, b]

let' :: [(String, Expr)] -> Expr -> Expr
let' = Let'

var :: String -> Expr
var = Var

lit :: String -> Expr
lit = Lit

nil :: Expr
nil = Nil

add :: Expr -> Expr -> Expr
add a b = call "+" [a, b]
