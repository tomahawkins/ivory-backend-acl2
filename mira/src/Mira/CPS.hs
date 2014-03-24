-- | A CPS IR.
module Mira.CPS
  ( Proc         (..)
  , Value        (..)
  , Literal      (..)
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
  = Var       Var         -- ^ A variable reference.
  | Literal   Literal     -- ^ A constant.
  | Pop                   -- ^ Pop a value off the stack.
  | Intrinsic Intrinsic [Var]   -- ^ An application of an intrinsic to a list of arguments.

instance Show Value where
  show a = case a of
    Var a            -> a
    Literal a        -> show a
    Pop              -> "pop"
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
  = Halt                            -- ^ End the program or loop.
  | Call    Var [Var] (Maybe Cont)  -- ^ Push the continuation onto the stack (if one exists) and call a function with arguments.
  | Return  (Maybe Var)             -- ^ Pop a continuation off the stack and execute it.  Saves the return value to the ReturnValue register.
  | Push    Var Cont                -- ^ Push a value onto the stack.
  | Let     Var Value Cont          -- ^ Brings a new variable into scope and assigns it a value.
  | If      Var Cont Cont           -- ^ Conditionally follow one continuation or another.
  | Assert  Var Cont                -- ^ Assert a value and continue.
  | Assume  Var Cont                -- ^ State an assumption and continue.

instance Show Cont where
  show a = case a of
    Halt                 -> "halt\n"
    Call    a b Nothing  -> printf "%s(%s)\n" a (intercalate ", " b)
    Call    a b (Just c) -> printf "%s(%s)\n%s" a (intercalate ", " b) (show c)
    Return  (Just a)     -> printf "return %s\n" a
    Return  Nothing      -> "return\n"
    Push    a b          -> printf "push %s\n" a ++ show b
    If      a b c        -> printf "if (%s)\n" a ++ indent (show b) ++ "\nelse\n" ++ indent (show c)
    Assert  a b          -> printf "assert %s\n%s" a (show b)
    Assume  a b          -> printf "assume %s\n%s" a (show b)
    Let     a b c        -> printf "let %s = %s\n%s" a (show b) (show c)

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
    Halt             -> []
    Call    _ a b    -> a ++ case b of { Nothing -> []; Just b -> cont b }
    Return  (Just a) -> [a]
    Return  Nothing  -> []
    Push    a b      -> a : cont b
    If      a b c    -> a : cont b ++ cont c
    Assert  a b      -> a : cont b
    Assume  a b      -> a : cont b
    Let     a b c    -> a : b' ++ cont c
      where
      b' = case b of
        Var a         -> [a]
        Literal _     -> []
        Pop           -> []
        Intrinsic _ a -> a

-- | All free (unbound) variables in a continuation.
contFreeVars :: Cont -> [Var]
contFreeVars = nub . cont ["retval"]
  where
  cont :: [Var] -> Cont -> [Var]
  cont i a = case a of
    Call    _ args a -> concatMap var args ++ case a of { Nothing -> []; Just a -> cont i a }
    Return  _        -> []
    Push    a b      -> var a ++ cont i b
    Let     a b c    -> vars ++ cont (a : i) c
      where
      vars = case b of
        Var       a   -> var a
        Literal   _   -> []
        Pop           -> []
        Intrinsic _ a -> concatMap var a
    Halt          -> []
    If      a b c -> var a ++ cont i b ++ cont i c
    Assert  a b   -> var a ++ cont i b
    Assume  a b   -> var a ++ cont i b
    where
    var :: Var -> [Var]
    var a = if elem a i then [] else [a]

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
    If      a b c -> If a (rep b) (rep c)
    Assert  a b   -> Assert a $ rep b
    Assume  a b   -> Assume a $ rep b
  where
  rep = replaceCont replacement

{-

Questions and Ideas:

Q: How should continuations be represented in ACL2?

One thought is to collect all the pushed continuations and turn them into functions.
To be compliant with ACL2's restriction on first class functions, a Push operation
would then push a corresponding code, not the function, onto the continuation stack.
On a Return, a code would be popped of the stack and appropriate continuation would be called.

Compiled Ivory procedures would take the continuation stack as an extra argument.

Branches on If statements would also be turned into continuation functions.
The If would then select and call the appropriate continuation.

Or perhaps, every continuation gets its own continuation function. 


Q: How to handle Ivory state in ACL2 (Store, Assign, etc)?

Since CPS turns everything into a tail call, I think state can be 
managed by passing the global state into every compiled Ivory function
and continuation function.


Q: How to handle loops in ACL2?

Fixed duration Loops can be rewritten Forever loops with conditional breaks.
Is it possible rewrite Forever loops into a combination of CPS tricks?


Q: How are assertions verified?

Are programs rewritten (truncated) to assertion point and verified independently?
Would ACL2 be smart enough to tie this information back to the larger program?

Or would the compiled programs return all the assertions collect throughout the execution,
which ACL2 would verify independently?  But this would still happen under one defthm,
so I'm not sure how the various assertions would serve as lemmas for larger proofs.

-}


