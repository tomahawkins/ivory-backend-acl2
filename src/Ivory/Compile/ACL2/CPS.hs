-- | A CPS IR.
module Ivory.Compile.ACL2.CPS
  ( Proc      (..)
  , Value     (..)
  , Literal   (..)
  , Cont      (..)
  , Var
  , contFreeVars
  , alphaConvert
  ) where

import Data.List
import Data.Maybe (fromJust)
import MonadLib
import Text.Printf

type Var = String

-- | A procedure is a name, a list of arguments, and its continuation (body).
data Proc a = Proc Var [Var] (Cont a)

instance Show a => Show (Proc a) where
  show (Proc name args body) = printf "%s(%s)\n%s\n" name (intercalate ", " args) (show body)

-- | Literals.
data Literal
  = LitInteger Integer
  | LitFloat Float
  | LitDouble Double
  | LitChar Char
  | LitBool Bool
  | LitNull
  | LitString String

instance Show Literal where
  show a = case a of
    LitInteger a -> show a
    LitFloat   a -> show a
    LitDouble  a -> show a
    LitChar    a -> show a
    LitBool    a -> show a
    LitString  a -> show a
    LitNull      -> "null"

-- | Values used in let bindings.
data Value a
  = Var       Var         -- ^ A variable reference.
  | Literal   Literal     -- ^ A constant.
  | Deref     Var         -- ^ Dereferences a pointer.
  | Intrinsic a [Var]     -- ^ An application of an intrinsic to a list of arguments.

instance Show a => Show (Value a) where
  show a = case a of
    Var a -> a
    Literal a -> show a
    Deref a -> "* " ++ a
    Intrinsic a args -> printf "(%s) (%s)" (show a) (intercalate ", " args)

-- | Continuations.
data Cont a
  = Halt                               -- ^ End the program or loop.
  | Call    Var [Var] (Cont a)         -- ^ Push the continuation onto the stack and call a function with arguments.
  | Pop     (Cont a)                   -- ^ Pop a continuation off the stack and throw it away. 
  | Return  (Maybe Var)                -- ^ Pop a continuation off the stack and execute it.  Saves the return value to the ReturnValue register.
  | Let     Var (Value a) (Cont a)     -- ^ Brings a new variable into scope and assigns it a value.
  | If      Var (Cont a) (Cont a)      -- ^ Conditionally follow one continuation or another.
  | Assert  Var (Cont a)               -- ^ Assert a value and continue.
  | Assume  Var (Cont a)               -- ^ State an assumption and continue.
  | Store   Var Var (Cont a)           -- ^ Store a value and continue.
  | Forever (Cont a) (Cont a)          -- ^ Loop forever on this continuation.
  | Loop    Var Var Bool Var (Cont a) (Cont a)  -- ^ Loop a fixed number of times: Loop loopVar initValue incrUp endValue body postLoop.

instance Show a => Show (Cont a) where
  show a = case a of
    Halt             -> "halt\n"
    Call    a b c    -> printf "%s(%s)\n%s" a (intercalate ", " b) (show c)
    Pop     a        -> "pop\n" ++ show a
    Return  (Just a) -> printf "return %s\n" a
    Return  Nothing  -> "return\n"
    If      a b c    -> printf "if (%s)\n" a ++ indent (show b) ++ "\nelse\n" ++ indent (show c)
    Assert  a b      -> printf "assert %s\n%s" a (show b)
    Assume  a b      -> printf "assume %s\n%s" a (show b)
    Let     a b c    -> printf "let %s = %s\n%s" a (show b) (show c)
    Store   _ _ _    -> error "Store not supported."
    Forever _ _      -> error "Forever not supported."
    Loop    _ _ _ _ _ _ -> error "Loop not supported."
    where
    indent :: String -> String
    indent = intercalate "\n" . map ("\t" ++) . lines
    

-- | All free (unbound) variables in a continuation.
contFreeVars :: Cont a -> [Var]
contFreeVars = nub . cont ["retval"]
  where
  cont :: [Var] -> Cont a -> [Var]
  cont i a = case a of
    Call    _ args a -> concatMap var args ++ cont i a
    Pop     a        -> cont i a
    Return  _        -> []
    Let     a b c    -> vars ++ cont (a : i) c
      where
      vars = case b of
        Var       a   -> var a
        Literal   _   -> []
        Deref     a   -> var a
        Intrinsic _ a -> concatMap var a
    Halt          -> []
    If      a b c -> var a ++ cont i b ++ cont i c
    Assert  a b   -> var a ++ cont i b
    Assume  a b   -> var a ++ cont i b
    Store   a b c -> var a ++ var b ++ cont i c
    Forever a b   -> cont i a ++ cont i b
    Loop    a b _ c d e -> var b ++ var c ++ cont (a : i) d ++ cont i e
    where
    var :: Var -> [Var]
    var a = if elem a i then [] else [a]

type Alpha = StateT (Int, [(Var, Var)]) Id

-- Creates a new name for an introduced variable.
newVar :: Var -> Alpha Var
newVar a = do
  (i, env) <- get
  let a' = "_cpsAlphaConvert" ++ show i ++ "_" ++ a
  set (i + 1, (a, a') : env)
  return a'

-- Replaces variable reference with new name.
ref :: Var -> Alpha Var
ref a = do
  (_, env) <- get
  case lookup a env of
    Nothing -> error $ "Variable not found: " ++ a ++ "\n" ++ unlines (map show env)
    Just a  -> return a

-- Removes a var from the environment.
popVar :: Var -> Alpha ()
popVar a = do
  (i, env) <- get
  let n = fromJust $ findIndex ((== a) . snd) env
  set (i, take n env ++ tail (drop n env))

-- | Alpha conversion makes all variables unique.
alphaConvert :: [Proc i] -> [Proc i]
alphaConvert procs = fst $ runId $ runStateT (0, undefined) $ mapM proc procs
  where
  proc :: Proc i -> Alpha (Proc i)
  proc (Proc name args body) = do
    (i, _) <- get
    set (i, [("retval", "retval")])
    args <- mapM newVar args
    body <- cont body
    return $ Proc name args body

  cont :: Cont i -> Alpha (Cont i)
  cont a = case a of
    Halt             -> return Halt
    Call    a b c    -> do { b <- mapM ref b; c <- cont c; return $ Call a b c }
    Pop     a        -> do { a <- cont a; return $ Pop a }
    Return  (Just a) -> do { a <- ref a; return $ Return $ Just a }
    Return  Nothing  -> do { return $ Return Nothing }
    If      a b c    -> do { a <- ref a; b <- cont b; c <- cont c; return $ If a b c }
    Assert  a b      -> do { a <- ref a; b <- cont b; return $ Assert a b }
    Assume  a b      -> do { a <- ref a; b <- cont b; return $ Assume a b }
    Store   a b c    -> do { a <- ref a; b <- ref b; c <- cont c; return $ Store a b c }
    Forever a b      -> do { a <- cont a; b <- cont b; return $ Forever a b }
    Loop    a b c d e f -> do { b <- ref b; d <- ref d; a <- newVar a; e <- cont e; popVar a; f <- cont f; return $ Loop a b c d e f }
    Let     a b c    -> do { b <- value b; a <- newVar a; c <- cont c; return $ Let a b c }
      where
      value :: Value i -> Alpha (Value i)
      value a = case a of
        Var       a   -> do { a <- ref a; return $ Var a }
        Literal   a   -> do { return $ Literal a }
        Deref     a   -> do { a <- ref a; return $ Deref a }
        Intrinsic i a -> do { a <- mapM ref a; return $ Intrinsic i a }


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


