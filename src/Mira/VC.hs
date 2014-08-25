module Mira.VC
  ( Expr (..)
  , UniOp (..)
  , BinOp (..)
  , optimize
  , let'
  , not'
  , (&&.)
  , (||.)
  , implies
  , true
  , false
  , if'
  , length'
  , (==.)
  , (<.)
  , (<=.)
  , (>.)
  , (>=.)
  , mod'
  ) where

import Data.List
import Text.Printf

data Expr
  = Var         String
  | ForAll      [String] Expr
  | RecordNil
  | RecordCons  String Expr Expr  -- fieldName fieldValue record
  | RecordProj  Expr String
  | ArrayNil
  | ArrayCons   Expr Expr  -- array item    Cons backwards to preserve indices.
  | ArrayProj   Expr Expr
  | ArrayUpdate Expr Expr Expr  -- index value array
  | Let         String Expr Expr
  | UniOp       UniOp Expr
  | BinOp       BinOp Expr Expr
  | If          Expr Expr Expr
  | Bool        Bool
  | Integer     Integer
  deriving Eq

data UniOp
  = Not
  | Length
  | Negate
  | Abs
  | Signum
  deriving Eq

data BinOp
  = And
  | Or
  | Implies
  | Eq
  | Lt
  | Gt
  | Add
  | Sub
  | Mul
  | Mod
  deriving Eq

instance Show Expr where
  show a = case a of
    Var         a     -> a
    ForAll      a b   -> printf "forall %s in\n%s" (intercalate " " a) (show b)
    RecordNil         -> printf "[]"
    RecordCons  a b c -> printf "(\"%s\", %s) : %s" a (show b) (show c)
    RecordProj  a b   -> printf "%s(\"%s\")" (show a) b
    ArrayNil          -> printf "[]"
    ArrayCons   a b   -> printf "%s : %s" (show a) (show b)
    ArrayProj   a b   -> printf "%s[%s]"  (show a) (show b)
    ArrayUpdate a b c -> printf "update %s %s %s" (show a) (show b) (show c)
    Let         a b c -> printf "let %s = %s in\n%s" a (show b) (show c)
    UniOp       a b   -> printf "(%s %s)" (show a) (show b)
    BinOp       a b c -> printf "(%s %s %s)" (show b) (show a) (show c)
    If          a b c -> printf "(if %s then %s else %s)" (show a) (show b) (show c)
    Bool        a     -> if a then "true" else "false"
    Integer a -> show a

instance Show UniOp where
  show a = case a of
    Not -> "!"
    Length -> "length"
    Negate -> "negate"
    Abs    -> "abs"
    Signum -> "signum"

instance Show BinOp where
  show a = case a of
    And     -> "&&"
    Or      -> "||"
    Implies -> "->"
    Eq      -> "=="
    Lt      -> "<"
    Gt      -> ">"
    Add     -> "+"
    Sub     -> "-"
    Mul     -> "*"
    Mod     -> "%"

instance Num Expr where
  (+)    = BinOp Add
  (-)    = BinOp Sub
  (*)    = BinOp Mul
  negate = UniOp Negate
  abs    = UniOp Abs
  signum = UniOp Signum
  fromInteger = Integer

let' = Let
not' = UniOp Not
(&&.) = BinOp And
(||.) = BinOp Or
implies = BinOp Implies
true  = Bool True
false = Bool False
if' = If
length' = UniOp Length
(==.) = BinOp Eq
(<.) = BinOp Lt
a <=. b = (a <. b) ||. (a ==. b)
(>.) = BinOp Gt
a >=. b = (a >. b) ||. (a ==. b)
mod' = BinOp Mod

optimize :: Expr -> Expr
optimize = optConstantProp

optConstantProp :: Expr -> Expr
optConstantProp a = case a of
  UniOp Not a -> case optimize a of
    Bool a -> Bool $ not a
    a      -> UniOp Not a

  BinOp And a b -> case (optimize a, optimize b) of
    (Bool a, Bool b) -> Bool $ a && b
    (Bool False, _)  -> false
    (_, Bool False)  -> false
    (Bool True, b)   -> b
    (a, Bool True)   -> a
    (a, b)
      | a == b       -> a
      | otherwise    -> BinOp And a b

  BinOp Or a b -> case (optimize a, optimize b) of
    (Bool a, Bool b) -> Bool $ a || b
    (Bool True, _)   -> true
    (_, Bool True)   -> true
    (Bool False, b)  -> b
    (a, Bool False)  -> a
    (a, b)
      | a == b       -> a
      | otherwise    -> BinOp Or a b

  BinOp Implies a b -> case (optimize a, optimize b) of
    (Bool a, Bool b) -> Bool $ not a || b
    (Bool False, _)  -> true
    (_, Bool True)   -> true
    (Bool True, b)   -> b
    (a, b)
      | a == b       -> true
      | otherwise    -> BinOp Implies a b

  If a b c -> case (optimize a, optimize b, optimize c) of
    (Bool a, b, c) -> if a then b else c
    (a, b, c)
      | b == c -> b
      | otherwise -> If a b c

  BinOp Eq a b -> case (optimize a, optimize b) of
    (Bool a, Bool b)       -> Bool $ a == b
    (Integer a, Integer b) -> Bool $ a == b
    (a, b)
      | a == b -> true
      | otherwise -> BinOp Eq a b

  BinOp Lt a b -> case (optimize a, optimize b) of
    (Integer a, Integer b) -> Bool $ a < b
    (a, b)
      | a == b -> false
      | otherwise -> BinOp Lt a b

  BinOp Gt a b -> case (optimize a, optimize b) of
    (Integer a, Integer b) -> Bool $ a > b
    (a, b)
      | a == b -> false
      | otherwise -> BinOp Gt a b

  BinOp Add a b -> case (optimize a, optimize b) of
    (Integer a, Integer b) -> Integer $ a + b
    (a, b) -> BinOp Add a b

  BinOp Sub a b -> case (optimize a, optimize b) of
    (Integer a, Integer b) -> Integer $ a - b
    (a, b) -> BinOp Sub a b

  BinOp Mul a b -> case (optimize a, optimize b) of
    (Integer a, Integer b) -> Integer $ a * b
    (a, b) -> BinOp Mul a b

  BinOp Mod a b -> case (optimize a, optimize b) of
    (Integer a, Integer b) -> Integer $ mod a b
    (a, b) -> BinOp Mod a b

  UniOp Negate a -> case optimize a of
    Integer a -> Integer $ negate a
    a -> UniOp Negate a

  UniOp Abs a -> case optimize a of
    Integer a -> Integer $ abs a
    a -> UniOp Abs a

  UniOp Signum a -> case optimize a of
    Integer a -> Integer $ signum a
    a -> UniOp Signum a

  a -> a

  where
  optimize = optConstantProp

