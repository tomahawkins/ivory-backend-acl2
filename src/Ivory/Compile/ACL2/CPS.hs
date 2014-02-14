-- | A CPS IR.
module Ivory.Compile.ACL2.CPS
  ( Proc      (..)
  , SValue    (..)
  , BValue    (..)
  , Literal   (..)
  , Cont      (..)
  , Var
  , contFreeVars
  , alphaConvert
  ) where

import Data.List (nub, findIndex)
import Data.Maybe (fromJust)
import MonadLib

type Var = String

-- | A procedure is a name, a list of arguments, and its continuation (body).
data Proc a = Proc Var [Var] (Cont a) deriving Show

-- | Small values are simple enough to become registers.
data SValue
  = Var Var             -- ^ A variable reference.
  | ReturnValue         -- ^ The return value of a function.
  deriving Show

-- | Big values are more complicated expressions used in let binding.
data BValue a
  = SValue    SValue      -- ^ An SValue.
  | Literal   Literal     -- ^ A constant.
  | Deref     SValue      -- ^ Dereferences a pointer.
  | Intrinsic a [SValue]  -- ^ An application of an intrinsic to a list of arguments.
  deriving Show

-- | Literals.
data Literal
  = LitInteger Integer
  | LitFloat Float
  | LitDouble Double
  | LitChar Char
  | LitBool Bool
  | LitNull
  | LitString String
  deriving Show

-- | Continuations.
data Cont a
  = Halt                               -- ^ End the program or loop.
  | Call    Var [SValue]               -- ^ Function call, given the function name and arguments.
  | Push    (Cont a) (Cont a)          -- ^ Push a continuation onto the stack.
  | Pop     (Cont a)                   -- ^ Pop a continuation off the stack and throw it away. 
  | Return  (Maybe SValue)             -- ^ Pop a continuation off the stack and execute it.  Saves the return value to the ReturnValue register.
  | Let     Var (BValue a) (Cont a)    -- ^ Brings a new variable into scope and assigns it a value.
  | If      SValue (Cont a) (Cont a)   -- ^ Conditionally follow one continuation or another.
  | Assert  SValue (Cont a)            -- ^ Assert a value and continue.
  | Assume  SValue (Cont a)            -- ^ State an assumption and continue.
  | Store   SValue SValue (Cont a)     -- ^ Store a value and continue.
  | Forever (Cont a)                   -- ^ Loop forever on this continuation.
  | Loop    Var SValue Bool SValue (Cont a) (Cont a)  -- ^ Loop a fixed number of times with a looping variable.
  deriving Show

-- | All free (unbound) variables in a continuation.
contFreeVars :: Cont a -> [Var]
contFreeVars = nub . cont []
  where
  cont :: [Var] -> Cont a -> [Var]
  cont i a = case a of
    Call    _ args        -> concatMap sValue args
    Push    a b           -> cont i a ++ cont i b
    Pop     a             -> cont i a
    Return (Just (Var a)) -> [a]
    Return _              -> []
    Let     a b c         -> bVars ++ cont (a : i) c
      where
      bVars = case b of
        SValue    a       -> sValue a
        Literal   _       -> []
        Deref     a       -> sValue a
        Intrinsic _ args  -> concatMap sValue args
    If      a b c -> sValue a ++ cont i b ++ cont i c
    Halt          -> []
    Assert  a b   -> sValue a ++ cont i b
    Assume  a b   -> sValue a ++ cont i b
    Store   a b c -> sValue a ++ sValue b ++ cont i c
    Forever a     -> cont i a
    Loop    a b _ c d e -> sValue b ++ sValue c ++ cont (a : i) d ++ cont i e
    where
    sValue :: SValue -> [Var]
    sValue a = case a of
      Var a
        | elem a i -> []
        | otherwise -> [a]
      _     -> []



type Alpha = StateT (Int, [(Var, Var)]) Id

-- Creates a new name for an introduced variable.
var :: Var -> Alpha Var
var a = do
  (i, env) <- get
  let a' = "_alpha_" ++ show i
  set (i + 1, (a, a') : env)
  return a'

-- Replaces variable reference with new name.
ref :: Var -> Alpha Var
ref a = do
  (_, env) <- get
  case lookup a env of
    Nothing -> error $ "Variable not found: " ++ a
    Just a  -> return a

popVar :: Var -> Alpha ()
popVar a = do
  (i, env) <- get
  let n = fromJust $ findIndex ((== a) . snd) env
  set (i, take n env ++ tail (drop n env))

-- | Alpha conversion makes all variables unique.
alphaConvert :: [Proc i] -> [Proc i]
alphaConvert procs = fst $ runId $ runStateT (0, []) $ mapM proc procs
  where
  proc :: Proc i -> Alpha (Proc i)
  proc (Proc name args body) = do
    (i, _) <- get
    set (i, [])
    args <- mapM var args
    body <- cont body
    return $ Proc name args body

  cont :: Cont i -> Alpha (Cont i)
  cont a = case a of
    Call    f args        -> mapM sValue args >>= return . Call f
    Push    a b           -> do { a <- cont a; b <- cont b; return $ Push a b }
    Pop     a             -> do { a <- cont a; return $ Pop a }
    Return  (Just a)      -> do { a <- sValue a; return $ Return $ Just a }
    Return  Nothing       -> return $ Return Nothing
    Let     a b c         -> do { b <- bValue b; a <- var a; c <- cont c; return $ Let a b c }
      where
      bValue :: BValue i -> Alpha (BValue i)
      bValue a = case a of
        SValue    a    -> do { a <- sValue a; return $ SValue a }
        Literal   a    -> return $ Literal a
        Deref     a    -> do { a <- sValue a; return $ Deref a }
        Intrinsic i a  -> do { a <- mapM sValue a; return $ Intrinsic i a }
    If      a b c -> do { a <- sValue a; b <- cont b; c <- cont c; return $ If a b c }
    Halt          -> return Halt
    Assert  a b   -> do { a <- sValue a; b <- cont b; return $ Assert a b }
    Assume  a b   -> do { a <- sValue a; b <- cont b; return $ Assume a b }
    Store   a b c -> do { a <- sValue a; b <- sValue b; c <- cont c; return $ Store a b c }
    Forever a     -> do { a <- cont a; return $ Forever a }
    Loop    a b c d e f -> do { b <- sValue b; d <- sValue d; a <- var a; e <- cont e; popVar a; f <- cont f; return $ Loop a b c d e f }

  sValue :: SValue -> Alpha SValue
  sValue a = case a of
    Var a -> ref a >>= return . Var
    a -> return a


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


