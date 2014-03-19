-- | A CPS IR.
module Ivory.Compile.ACL2.CPS
  ( Proc      (..)
  , Value     (..)
  , Literal   (..)
  , Cont      (..)
  , Var
  , variables
  , contFreeVars
  , alphaConvert
  , explicitStack
  ) where

import Data.List
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
  deriving Eq

instance Show Literal where
  show a = case a of
    LitInteger a -> show a
    LitFloat   a -> show a
    LitDouble  a -> show a
    LitChar    a -> show a
    LitBool    a -> show a
    LitString  a -> show a
    LitNull      -> "null"

instance Num Literal where
  (+)    = error "Method not supported for Num Literal."
  (-)    = error "Method not supported for Num Literal."
  (*)    = error "Method not supported for Num Literal."
  negate = error "Method not supported for Num Literal."
  abs    = error "Method not supported for Num Literal."
  signum = error "Method not supported for Num Literal."
  fromInteger = LitInteger

-- | Values used in let bindings.
data Value a
  = Var       Var         -- ^ A variable reference.
  | Literal   Literal     -- ^ A constant.
  | Pop                   -- ^ Pop a value off the stack.
  | Intrinsic a [Var]     -- ^ An application of an intrinsic to a list of arguments.

instance Show a => Show (Value a) where
  show a = case a of
    Var a            -> a
    Literal a        -> show a
    Pop              -> "pop"
    Intrinsic a args -> printf "(%s) (%s)" (show a) (intercalate ", " args)

-- | Continuations.
data Cont a
  = Halt                                -- ^ End the program or loop.
  | Call    Var [Var] (Maybe (Cont a))  -- ^ Push the continuation onto the stack (if one exists) and call a function with arguments.
  | Return  (Maybe Var)                 -- ^ Pop a continuation off the stack and execute it.  Saves the return value to the ReturnValue register.
  | Push    Var (Cont a)                -- ^ Push a value onto the stack.
  | Let     Var (Value a) (Cont a)      -- ^ Brings a new variable into scope and assigns it a value.
  | If      Var (Cont a) (Cont a)       -- ^ Conditionally follow one continuation or another.
  | Assert  Var (Cont a)                -- ^ Assert a value and continue.
  | Assume  Var (Cont a)                -- ^ State an assumption and continue.

instance Show a => Show (Cont a) where
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
    where
    indent :: String -> String
    indent = intercalate "\n" . map ("\t" ++) . lines

-- | All the variables in a CPS program.
variables :: [Proc i] -> [Var]
variables = nub . ("retval" :) . concatMap proc
  where
  proc :: Proc i -> [Var]
  proc (Proc _ args body) = args ++ cont body

  cont :: Cont i -> [Var]
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
contFreeVars :: Cont a -> [Var]
contFreeVars = nub . cont ["retval"]
  where
  cont :: [Var] -> Cont a -> [Var]
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
    Call    a b Nothing    -> do { b <- mapM ref b;              return $ Call a b Nothing }
    Call    a b (Just c)   -> do { b <- mapM ref b; c <- cont c; return $ Call a b $ Just c }
    Return  (Just a) -> do { a <- ref a; return $ Return $ Just a }
    Return  Nothing  -> do { return $ Return Nothing }
    Push    a b      -> do { a <- ref a; b <- cont b; return $ Push a b }
    If      a b c    -> do { a <- ref a; b <- cont b; c <- cont c; return $ If a b c }
    Assert  a b      -> do { a <- ref a; b <- cont b; return $ Assert a b }
    Assume  a b      -> do { a <- ref a; b <- cont b; return $ Assume a b }
    Let     a b c    -> do { b <- value b; a <- newVar a; c <- cont c; return $ Let a b c }
      where
      value :: Value i -> Alpha (Value i)
      value a = case a of
        Var       a   -> do { a <- ref a; return $ Var a }
        Literal   a   -> do { return $ Literal a }
        Pop           -> do { return Pop }
        Intrinsic i a -> do { a <- mapM ref a; return $ Intrinsic i a }


-- | Convert a procedure to make explicit use of the stack to save and restore variables across procedure calls.
explicitStack :: Proc i -> Proc i
explicitStack (Proc name args body) = Proc name args $ cont body
  where
  cont :: Cont i -> Cont i
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


