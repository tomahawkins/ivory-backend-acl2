module Mira.VC
  ( Expr (..)
  , UniOp (..)
  , BinOp (..)
  , optimize
  , let'
  , forAll
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
  = Var           String
  | ForAll        String Expr
  | Let           String Expr Expr
  | Record        [(String, Expr)]
  | RecordOverlay Expr Expr       -- overlay first over second
  | RecordProject Expr String
  | Array         [Expr]
  | ArrayAppend   Expr Expr
  | ArrayProject  Expr Expr
  | ArrayUpdate   Expr Expr Expr  -- index newValue oldArray
  | UniOp         UniOp Expr
  | BinOp         BinOp Expr Expr
  | If            Expr Expr Expr
  | Bool          Bool
  | Integer       Integer
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
    Var           a     -> a
    ForAll        a b   -> printf "forall %s in\n%s" a (show b)
    Record        a     -> printf "{%s}" $ intercalate ", " [ a ++ " : " ++ show b | (a, b) <- a ]
    RecordOverlay a b   -> printf "(overlay %s %s)" (show a) (show b)
    RecordProject a b   -> printf "%s.%s" (show a) b
    Array         a     -> printf "[%s]" $ intercalate ", " $ map show a
    ArrayAppend   a b   -> printf "(%s ++ %s)" (show a) (show b)
    ArrayUpdate   a b c -> printf "(update %s %s %s)" (show a) (show b) (show c)
    ArrayProject  a b   -> printf "%s[%s]"  (show a) (show b)
    Let           a b c -> printf "let %s = %s in\n%s" a (show b) (show c)
    UniOp         a b   -> printf "(%s %s)" (show a) (show b)
    BinOp         a b c -> printf "(%s %s %s)" (show b) (show a) (show c)
    If            a b c -> printf "(if %s then %s else %s)" (show a) (show b) (show c)
    Bool          a     -> if a then "true" else "false"
    Integer       a     -> show a

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

forAll :: [String] -> Expr -> Expr
forAll a b = case a of
  [] -> b
  a : rest -> ForAll a $ forAll rest b

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
optimize = optRemoveNullEffect . optConstantProp

optRemoveNullEffect :: Expr -> Expr
optRemoveNullEffect a = case a of
  Let a b c
    | elem a $ vars $ opt c -> Let a (opt b) (opt c)
    | otherwise -> opt c
  ForAll a b
    | elem a $ vars $ opt b -> ForAll a $ opt b
    | otherwise -> opt b
  Var a -> Var a
  If a b c -> If (opt a) (opt b) (opt c)
  UniOp a b -> UniOp a (opt b)
  BinOp a b c -> BinOp a (opt b) (opt c)
  Bool    a -> Bool a
  Integer a -> Integer a
  Record        a     -> Record [ (a, opt b) | (a, b) <- a ]
  RecordOverlay a b   -> RecordOverlay (opt a) (opt b)
  RecordProject a b   -> RecordProject (opt a) b
  Array         a     -> Array        (map opt a)
  ArrayAppend   a b   -> ArrayAppend  (opt a) (opt b)
  ArrayUpdate   a b c -> ArrayUpdate  (opt a) (opt b) (opt c)
  ArrayProject  a b   -> ArrayProject (opt a) (opt b)
  where
  opt = optRemoveNullEffect

  vars :: Expr -> [String]
  vars a = case a of
    Var    a     -> [a]
    Let    a b c -> vars b ++ [ v | v <- vars c, v /= a ]
    ForAll a b   -> [ v | v <- vars b, v /= a ]
    If a b c -> vars a ++ vars b ++ vars c
    UniOp _ a -> vars a
    BinOp _ a b -> vars a ++ vars b
    Bool _ -> []
    Integer _ -> []
    Record        a     -> concatMap vars $ snd $ unzip a
    RecordOverlay a b   -> vars a ++ vars b
    RecordProject a _   -> vars a
    Array         a     -> concatMap vars a
    ArrayAppend   a b   -> vars a ++ vars b
    ArrayProject  a b   -> vars a ++ vars b
    ArrayUpdate   a b c -> vars a ++ vars b ++ vars c

optConstantProp :: Expr -> Expr
optConstantProp = optConstantProp' []

optConstantProp' :: [(String, Expr)] -> Expr -> Expr
optConstantProp' env a = case a of
  UniOp Not a -> case opt a of
    Bool a -> Bool $ not a
    a      -> UniOp Not a

  BinOp And a b -> case (opt a, opt b) of
    (Bool a, Bool b) -> Bool $ a && b
    (Bool False, _)  -> false
    (_, Bool False)  -> false
    (Bool True, b)   -> b
    (a, Bool True)   -> a
    (a, b)
      | a == b       -> a
      | otherwise    -> BinOp And a b

  BinOp Or a b -> case (opt a, opt b) of
    (Bool a, Bool b) -> Bool $ a || b
    (Bool True, _)   -> true
    (_, Bool True)   -> true
    (Bool False, b)  -> b
    (a, Bool False)  -> a
    (a, b)
      | a == b       -> a
      | otherwise    -> BinOp Or a b

  BinOp Implies a b -> case (opt a, opt b) of
    (Bool a, Bool b) -> Bool $ not a || b
    (Bool False, _)  -> true
    (_, Bool True)   -> true
    (Bool True, b)   -> b
    (a, b)
      | a == b       -> true
      | otherwise    -> BinOp Implies a b

  BinOp Eq a b -> case (opt a, opt b) of
    (Bool a, Bool b)       -> Bool $ a == b
    (Integer a, Integer b) -> Bool $ a == b
    (a, b)
      | a == b -> true
      | otherwise -> BinOp Eq a b

  BinOp Lt a b -> case (opt a, opt b) of
    (Integer a, Integer b) -> Bool $ a < b
    (a, b)
      | a == b -> false
      | otherwise -> BinOp Lt a b

  BinOp Gt a b -> case (opt a, opt b) of
    (Integer a, Integer b) -> Bool $ a > b
    (a, b)
      | a == b -> false
      | otherwise -> BinOp Gt a b

  BinOp Add a b -> case (opt a, opt b) of
    (Integer a, Integer b) -> Integer $ a + b
    (a, b) -> BinOp Add a b

  BinOp Sub a b -> case (opt a, opt b) of
    (Integer a, Integer b) -> Integer $ a - b
    (a, b) -> BinOp Sub a b

  BinOp Mul a b -> case (opt a, opt b) of
    (Integer a, Integer b) -> Integer $ a * b
    (a, b) -> BinOp Mul a b

  BinOp Mod a b -> case (opt a, opt b) of
    (Integer a, Integer b) -> Integer $ mod a b
    (a, b) -> BinOp Mod a b

  UniOp Negate a -> case opt a of
    Integer a -> Integer $ negate a
    a -> UniOp Negate a

  UniOp Abs a -> case opt a of
    Integer a -> Integer $ abs a
    a -> UniOp Abs a

  UniOp Signum a -> case opt a of
    Integer a -> Integer $ signum a
    a -> UniOp Signum a

  UniOp Length a -> case opt a of
    Array a -> Integer $ fromIntegral $ length a
    a -> UniOp Length a

  Var    a     -> case lookup a env of
    Nothing -> Var a
    Just a  -> a

  Let    a b c -> case opt b of
    Bool    b -> opt' ((a, Bool    b) : env) c
    Integer b -> opt' ((a, Integer b) : env) c
    Record  b -> opt' ((a, Record  b) : env) c
    Array   b -> opt' ((a, Array   b) : env) c
    b -> Let a b (opt c)

  ForAll a b   -> ForAll a $ opt' [ (a', b) | (a', b) <- env, a' /= a ]  b

  If a b c -> case (opt a, opt b, opt c) of
    (Bool a, b, c) -> if a then b else c
    (a, b, c)
      | b == c -> b
      | otherwise -> If a b c

  Record        a     -> Record [ (a, opt b) | (a, b) <- a ]
  RecordOverlay a b   -> case (opt a, opt b) of
    (Record a, Record b) -> Record $ a ++ [ b | b <- b, notElem (fst b) $ fst $ unzip a ]
    (a, b)               -> RecordOverlay a b
  RecordProject a b   -> case opt a of
    Record a -> case lookup b a of
      Nothing -> error $ printf "Record %s doesn't have field '%s'." (show (Record a)) b
      Just b  -> b
    a -> RecordProject a b

  Array       a     -> Array $ map opt a
  ArrayAppend a b   -> case (opt a, opt b) of
    (Array a, Array b) -> Array $ a ++ b
    (a, b)             -> ArrayAppend a b

  ArrayProject a b   -> case (opt a, opt b) of
    (Integer a', Array b)
      | a < length b -> b !! a
      | otherwise -> error "Index exceeds bounds of array."
      where
      a = fromInteger a'
    (a, b) -> ArrayProject a b

  ArrayUpdate  a b c -> case (opt a, opt b, opt c) of
    (Integer a', b, Array c)
      | a < length c -> Array $ take a c ++ [b] ++ drop (a + 1) c
      | otherwise -> error "Index exceeds bounds of array."
      where
      a = fromInteger a'
    (a, b, c) -> ArrayUpdate a b c

  Bool    a -> Bool a
  Integer a -> Integer a

  where
  opt  = optConstantProp' env
  opt' = optConstantProp'

