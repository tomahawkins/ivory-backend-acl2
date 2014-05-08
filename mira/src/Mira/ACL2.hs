-- | A DSL for ACL2.
module Mira.ACL2
  ( Expr (..)
  , mutualRecursion
  , defun
  , defun'
  , defconst
  , defthm
  , call
  , obj
  , quote
  , consp
  , cons
  , car
  , cdr
  , nth
  , len
  , take'
  , nthcdr
  , append
  , let'
  , if'
  , case'
  , var
  , lit
  , nil
  , t
  , zp
  , zip'
  , undefined'
  , equal
  , not'
  , and'
  , or'
  , goodbye
  , integerp
  , mod'
  ) where

data SExpr
  = SV String
  | SA [SExpr]

instance Show SExpr where
  show a = case a of
    SV a -> a ++ "\n"
    SA args -> "( " ++ indent (concatMap show args) ++ ")\n"
      where
      indent = drop 2 . unlines . map ("  " ++) . lines

data Expr
  = Obj [Expr]
  | Lit String
  deriving (Eq, Ord)

instance Show Expr where show = show . sExpr

instance Num Expr where
  a + b = call "+" [a, b]
  a - b = call "-" [a, b]
  a * b = call "*" [a, b]
  negate a = 0 - a
  abs = undefined
  signum = undefined
  fromInteger = Lit . show

sExpr :: Expr -> SExpr
sExpr a = case a of
  Obj     a   -> SA $ map sExpr a
  Lit     a   -> SV a

mutualRecursion :: [Expr] -> Expr
mutualRecursion = call "mutual-recursion"

defun :: String -> [String] -> Expr -> Expr
defun name args body = call "defun" [var name, obj $ map var args, body]

defun' :: String -> [String] -> Expr -> Expr -> Expr
defun' name args measure body = call "defun"
  [ var name
  , obj $ map var args
  , call "declare" [call "xargs" [var ":measure", call "nfix" [measure]]]
  , body
  ]

defconst :: String -> Expr -> Expr
defconst name const = call "defconst" [var name, const]

defthm :: String -> Expr -> Expr
defthm name a = call "defthm" [var name, a]
 
call :: String -> [Expr] -> Expr
call a b = Obj $ var a : b

obj :: [Expr] -> Expr
obj = Obj

quote :: Expr -> Expr
quote a = call "quote" [a]

consp :: Expr -> Expr
consp a = call "consp" [a]

cons :: Expr -> Expr -> Expr
cons a b = call "cons" [a, b]

car :: Expr -> Expr
car a = call "car" [a]

cdr :: Expr -> Expr
cdr a = call "cdr" [a]

nth :: Expr -> Expr -> Expr
nth a b = call "nth" [a, b]

len :: Expr -> Expr
len a = call "len" [a]

take' :: Expr -> Expr -> Expr
take' a b = call "take" [a, b]

nthcdr :: Expr -> Expr -> Expr
nthcdr a b = call "nthcdr" [a, b]

append :: Expr -> Expr -> Expr
append a b = call "append" [a, b]

let' :: [(String, Expr)] -> Expr -> Expr
let' a b = case b of
  Obj [Lit "let*", Obj lets, expr] -> call "let*" [obj $ [ obj [var n, e] | (n, e) <- a ] ++ lets, expr]
  _ -> call "let*" [obj [ obj [var n, e] | (n, e) <- a ], b]

var :: String -> Expr
var = Lit

lit :: String -> Expr
lit = Lit

nil :: Expr
nil = Lit "nil"

t :: Expr
t = Lit "t"

if' :: Expr -> Expr -> Expr -> Expr
if' a b c = call "if" [a, b, c]

case' :: Expr -> [(Expr, Expr)] -> Expr -> Expr
case' a b c = call "case" $ a : [ obj [a, b] | (a, b) <- b ] ++ [call "otherwise" [c]]

zp :: Expr -> Expr
zp a = call "zp" [a]

zip' :: Expr -> Expr
zip' a = call "zip" [a]

integerp :: Expr -> Expr
integerp a = call "integerp" [a]

undefined' :: Expr
undefined' = Lit "undefined"

equal :: Expr -> Expr -> Expr
equal a b = call "equal" [a, b]

not' :: Expr -> Expr
not' a = call "not" [a]

and' :: Expr -> Expr -> Expr
and' a b = call "and" [a, b]

or' :: Expr -> Expr -> Expr
or' a b = call "or" [a, b]

goodbye :: Expr
goodbye = call "good-bye" []

mod' :: Expr -> Expr -> Expr
mod' a b = call "mod" [a, b]

