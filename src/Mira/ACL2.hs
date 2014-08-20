-- | A DSL for ACL2.
module Mira.ACL2
  ( Expr (..)
  , check
  , check'
  , mutualRecursion
  , defun
  , defun'
  , defconst
  , defthm
  , thm
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
  , updateNth
  , append
  , let'
  , if'
  , case'
  , assoc
  , var
  , list
  , lit
  , string
  , nil
  , t
  , zp
  , zip'
  , undefined'
  , equal
  , not'
  , and'
  , or'
  , implies
  , goodbye
  , integerp
  , mod'
  , (<.)
  , (>.)
  , (<=.)
  , (>=.)
  ) where

import Data.List
import System.Environment
import System.Process

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

check'' :: Bool -> [Expr] -> IO Bool
check'' debug a = do
  exe <- savedACL2
  (_, result, _) <- readProcessWithExitCode exe [] code
  let pass = not $ any (isPrefixOf "ACL2 Error") $ lines result
  if debug then putStrLn result else return ()
  return pass
  where
  code = unlines $ map show a
  savedACL2 :: IO FilePath
  savedACL2 = do
    env <- getEnvironment
    case lookup "ACL2_SOURCES" env of
      Nothing -> error "Environment variable ACL2_SOURCES not set."
      Just a -> return $ a ++ "/saved_acl2"

check :: [Expr] -> IO Bool
check = check'' False

check' :: [Expr] -> IO Bool
check' = check'' True

uni :: String -> Expr -> Expr
uni f a = call f [a]

bin :: String -> Expr -> Expr -> Expr
bin f a b = call f [a, b]

tri :: String -> Expr -> Expr -> Expr -> Expr
tri f a b c = call f [a, b, c]

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

thm :: Expr -> Expr
thm = uni "thm"
 
call :: String -> [Expr] -> Expr
call a b = Obj $ var a : b

obj :: [Expr] -> Expr
obj = Obj

quote :: Expr -> Expr
quote = uni "quote"

list :: [Expr] -> Expr
list a = call "list" a

consp :: Expr -> Expr
consp = uni "consp"

cons :: Expr -> Expr -> Expr
cons = bin "cons"

car :: Expr -> Expr
car = uni "car"

cdr :: Expr -> Expr
cdr = uni "cdr"

nth :: Expr -> Expr -> Expr
nth = bin "nth"

len :: Expr -> Expr
len = uni "len"

take' :: Expr -> Expr -> Expr
take' = bin "take"

nthcdr :: Expr -> Expr -> Expr
nthcdr = bin "nthcdr"

updateNth :: Expr -> Expr -> Expr -> Expr
updateNth = tri "update-nth"

append :: Expr -> Expr -> Expr
append = bin "append"

let' :: [(String, Expr)] -> Expr -> Expr
let' a b = case b of
  Obj [Lit "let*", Obj lets, expr] -> call "let*" [obj $ [ obj [var n, e] | (n, e) <- a ] ++ lets, expr]
  _ -> call "let*" [obj [ obj [var n, e] | (n, e) <- a ], b]

var :: String -> Expr
var = Lit

lit :: String -> Expr
lit = Lit

string :: String -> Expr
string = Lit . show

nil :: Expr
nil = Lit "nil"

t :: Expr
t = Lit "t"

if' :: Expr -> Expr -> Expr -> Expr
if' = tri "if"

case' :: Expr -> [(Expr, Expr)] -> Expr -> Expr
case' a b c = call "case" $ a : [ obj [a, b] | (a, b) <- b ] ++ [call "otherwise" [c]]

zp :: Expr -> Expr
zp = uni "zp"

zip' :: Expr -> Expr
zip' = uni "zip"

integerp :: Expr -> Expr
integerp = uni "integerp"

undefined' :: Expr
undefined' = Lit "undefined"

equal :: Expr -> Expr -> Expr
equal = bin "equal"

not' :: Expr -> Expr
not' = uni "not"

and' :: Expr -> Expr -> Expr
and' = bin "and"

or' :: Expr -> Expr -> Expr
or' = bin "or"

implies :: Expr -> Expr -> Expr
implies = bin "implies"

goodbye :: Expr
goodbye = call "good-bye" []

mod' :: Expr -> Expr -> Expr
mod' = bin "mod"

(<.) :: Expr -> Expr -> Expr
(<.) = bin "<"

(<=.) :: Expr -> Expr -> Expr
(<=.) = bin "<="

(>.) :: Expr -> Expr -> Expr
(>.) = bin ">"

(>=.) :: Expr -> Expr -> Expr
(>=.) = bin ">="

assoc :: Expr -> Expr -> Expr
assoc = bin "assoc"

