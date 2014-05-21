-- | A CPS IR.
module Mira.CPS
  ( Proc         (..)
  , Value        (..)
  , Literal      (..)
  , StoreTo      (..)
  , Cont         (..)
  , Var
  , variables
  , contFreeVars
  , explicitStack
  , replaceCont
  ) where

import Data.List
import Text.Printf

import Mira.Expr hiding (Expr (..))
import qualified Mira.Expr as E

-- | A procedure is a name, a list of arguments, an optional measure, and its continuation (body).
data Proc = Proc Var [Var] (Maybe E.Expr) Cont

instance Show Proc where
  show (Proc name args Nothing  body) = printf "%s(%s)\n%s\n" name (intercalate ", " args) (indent $ show body)
  show (Proc name args (Just m) body) = printf "%s(%s)\n\tmeasure %s\n%s\n" name (intercalate ", " args) (show m) (indent $ show body)

-- | Values used in let bindings.
data Value
  = Var         Var               -- ^ A variable reference.
  | Literal     Literal           -- ^ A constant.
  | Pop                           -- ^ Pop a value off the stack.
  | Deref       Var               -- ^ Dereference a ref.
  | Alloc       Int               -- ^ Allocate space from the heap.
  | Array       [Var]             -- ^ Array construction.
  | Struct      [(String, Var)]   -- ^ Struct construction.
  | ArrayIndex  Var Var           -- ^ Array indexing.
  | StructIndex Var String        -- ^ Structure indexing.
  | Intrinsic   Intrinsic [Var]   -- ^ An application of an intrinsic to a list of arguments.

instance Show Value where
  show a = case a of
    Var a            -> a
    Literal a        -> show a
    Pop              -> "pop"
    Deref a          -> "deref " ++ a
    Alloc a          -> printf "alloc %d" a
    Array a          -> printf "array [%s]" $ intercalate ", " a
    Struct a         -> printf "struct {%s}" $ intercalate ", " [ printf "%s: %s" n v | (n, v) <- a ]
    ArrayIndex a b   -> printf "%s[%s]" a b
    StructIndex a b  -> printf "%s.%s"  a b
    Intrinsic a args -> printf "%s(%s)" (show a) (intercalate ", " args)

instance Num Value where
  (+)    = error "Method not supported for Num Value."
  (-)    = error "Method not supported for Num Value."
  (*)    = error "Method not supported for Num Value."
  negate = error "Method not supported for Num Value."
  abs    = error "Method not supported for Num Value."
  signum = error "Method not supported for Num Value."
  fromInteger = Literal . fromInteger

-- | Continuations.
data Cont
  = Halt                             -- ^ End the program or loop.
  | Call     Var [Var] (Maybe Cont)  -- ^ Push the continuation onto the stack (if one exists) and call a function with arguments.
  | Return   (Maybe Var)             -- ^ Pop a continuation off the stack and execute it.  Saves the return value to the ReturnValue register.
  | Push     Var Cont                -- ^ Push a value onto the stack.
  | Let      Var Value Cont          -- ^ Brings a new variable into scope and assigns it a value.
  | Store    StoreTo Var Cont        -- ^ Store a value to a ref.
  | If       Var Cont Cont           -- ^ Conditionally follow one continuation or another.
  | Assert   Var Cont                -- ^ Assert a value and continue.
  | Assume   Var Cont                -- ^ State an assumption and continue.

data StoreTo
  = SRef         Var
  | SArrayIndex  Var Var
  | SStructField Var String

instance Show Cont where
  show a = case a of
    Halt                  -> "halt\n"
    Call     a b Nothing  -> printf "%s(%s)\n" a (intercalate ", " b)
    Call     a b (Just c) -> printf "%s(%s)\n%s" a (intercalate ", " b) (show c)
    Return   (Just a)     -> printf "return %s\n" a
    Return   Nothing      -> "return\n"
    Push     a b          -> printf "push %s\n" a ++ show b
    If       a b c        -> printf "if (%s)\n" a ++ indent (show b) ++ "\nelse\n" ++ indent (show c)
    Assert   a b          -> printf "assert %s\n%s" a (show b)
    Assume   a b          -> printf "assume %s\n%s" a (show b)
    Let      a b c        -> printf "let %s = %s\n%s" a (show b) (show c)
    Store    a b c        -> printf "store %s = %s\n%s" a' b (show c)
      where
      a' = case a of
        SRef         a   -> a
        SArrayIndex  a b -> printf "%s[%s]" (show a) (show b)
        SStructField a b -> printf "%s.%s" (show a) b

indent :: String -> String
indent = intercalate "\n" . map ("\t" ++) . lines

-- | All the variables in a CPS program.
variables :: [Proc] -> [Var]
variables = nub . ("retval" :) . concatMap proc
  where
  proc :: Proc -> [Var]
  proc (Proc _ args _ body) = args ++ cont body

  cont :: Cont -> [Var]
  cont a = case a of
    Halt              -> []
    Call     _ a b    -> a ++ case b of { Nothing -> []; Just b -> cont b }
    Return   (Just a) -> [a]
    Return   Nothing  -> []
    Push     a b      -> a : cont b
    If       a b c    -> a : cont b ++ cont c
    Assert   a b      -> a : cont b
    Assume   a b      -> a : cont b
    Let      a b c    -> a : value b ++ cont c
    Store    a b c    -> storeTo a ++ [b] ++ cont c

  value :: Value -> [Var]
  value a = case a of
    Var a         -> [a]
    Literal _     -> []
    Pop           -> []
    Deref a       -> [a]
    Alloc _       -> []
    Array a       -> a
    Struct a      -> snd $ unzip a
    ArrayIndex a b -> [a, b]
    StructIndex a _ -> [a]
    Intrinsic _ a -> a

  storeTo :: StoreTo -> [Var]
  storeTo a = case a of
    SRef         a   -> [a]
    SArrayIndex  a b -> [a, b]
    SStructField a _ -> [a]

-- | All free (unbound) variables in a continuation.
contFreeVars :: Cont -> [Var]
contFreeVars = nub . cont ["retval"]
  where
  cont :: [Var] -> Cont -> [Var]
  cont i a = case a of
    Call     _ a b -> concatMap var a ++ case b of { Nothing -> []; Just a -> cont i a }
    Return   _     -> []
    Push     a b   -> var a ++ cont i b
    Let      a b c -> value b ++ cont (a : i) c
    Store    a b c -> storeTo a ++ var b ++ cont i c
    Halt           -> []
    If       a b c -> var a ++ cont i b ++ cont i c
    Assert   a b   -> var a ++ cont i b
    Assume   a b   -> var a ++ cont i b
    where
    var :: Var -> [Var]
    var a = if elem a i then [] else [a]

    value :: Value -> [Var]
    value a = case a of
      Var       a   -> var a
      Literal   _   -> []
      Pop           -> []
      Deref     a   -> var a
      Alloc     _   -> []
      Array      a   -> concatMap var a
      Struct     a   -> concatMap var $ snd $ unzip a
      ArrayIndex a b -> var a ++ var b
      StructIndex a _ -> var a
      Intrinsic _ a -> concatMap var a

    storeTo :: StoreTo -> [Var]
    storeTo a = case a of
      SRef         a   -> var a
      SArrayIndex  a b -> var a ++ var b
      SStructField a _ -> var a

-- | Convert a procedure to make explicit use of the stack to save and restore variables across procedure calls.
explicitStack :: Proc -> Proc
explicitStack (Proc name args measure body) = Proc name args measure $ cont body
  where
  cont :: Cont -> Cont
  cont a = case a of
    Call a b (Just c) -> f1 freeVars
      where
      freeVars = contFreeVars c
      f1 vars = case vars of
        [] -> Call a b $ Just $ f2 $ reverse freeVars
        a : b -> Push a $ f1 b
      f2 vars = case vars of
        [] -> cont c
        a : b -> Let a Pop $ f2 b
    Call    a b Nothing -> Call a b Nothing
    Halt                -> Halt
    Return  a           -> Return a
    Push    a b         -> Push a $ cont b
    If      a b c       -> If a (cont b) (cont c)
    Assert  a b         -> Assert a $ cont b
    Assume  a b         -> Assume a $ cont b
    Let     a b c       -> Let a b $ cont c
    Store   a b c       -> Store a b $ cont c

-- | Replace a continuation given a pattern to match.
replaceCont :: (Cont -> Maybe Cont) -> Cont -> Cont
replaceCont replacement a = case replacement a of
  Just a  -> a
  Nothing -> case a of
    Halt          -> Halt
    Call    a b c -> Call a b c
    Return  a     -> Return a
    Push    a b   -> Push a $ rep b
    Let     a b c -> Let a b $ rep c
    Store   a b c -> Store a b $ rep c
    If      a b c -> If a (rep b) (rep c)
    Assert  a b   -> Assert a $ rep b
    Assume  a b   -> Assume a $ rep b
  where
  rep = replaceCont replacement

