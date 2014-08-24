-- | Verifies and removes assertions from an Ivory program.
module Ivory.Opts.Asserts
  ( assertsFold
  ) where

import MonadLib
import Data.List
import Text.Printf

import qualified Ivory.Language.Syntax.AST  as I
import qualified Ivory.Language.Syntax.Type as I
import Ivory.Compile.ACL2 (var, lit)
import qualified Mira.Expr  as M
import qualified Mira.ACL2 as A

-- Data carried through verification monad.
data VDB = VDB
  { nextVCId    :: Int
  , nextBCId    :: Int
  , nextFreeId  :: Int
  , nextEnvId   :: Int
  , nextStateId :: Int
  , procName    :: String
  , body        :: Expr -> Expr
  , branch      :: Expr
  , lemmas      :: [Expr]
  , env         :: Expr
  , state       :: Expr
  }

-- Verification monad.
type V a = StateT VDB IO a

-- | Analyze an entire Ivory program, removing as many assertions as possible.
assertsFold :: [I.Module] -> IO [I.Module]
assertsFold modules = mapM analyzeModule modules
  where
  analyzeModule :: I.Module -> IO I.Module
  analyzeModule m = do
    pub <- mapM analyzeProc $ I.public  $ I.modProcs m
    pri <- mapM analyzeProc $ I.private $ I.modProcs m
    return m { I.modProcs = I.Visible pub pri }
    where
    analyzeProc :: I.Proc -> IO I.Proc
    analyzeProc proc = do
      b <- runStateT init $ do
        mapM_ require $ I.procRequires proc
        block $ I.procBody proc
      return $ proc { I.procBody = fst b }
      where
      args = map (var . I.tValue) $ I.procArgs proc
      init = VDB
        { nextVCId    = 0
        , nextBCId    = 0
        , nextFreeId  = 0
        , nextEnvId   = 1
        , nextStateId = 1
        , procName    = I.procSym proc
        , body        = ForAll ("state0" : args) . let' "env0" (initEnv args)
        , branch      = true
        , lemmas      = []
        , env         = Var "env0"
        , state       = Var "state0"
        }
      initEnv :: [String] -> Expr
      initEnv a = case a of
        [] -> RecordNil
        a : b -> RecordCons a (Var a) $ initEnv b

-- Convert requires to lemmas.
require :: I.Require -> V ()
require (I.Require a) = do
  env <- getEnv
  a <- cond a
  setEnv env
  m <- get
  set m { lemmas = lemmas m ++ [a] }
  where
  cond :: I.Cond -> V Expr
  cond a = case a of
    I.CondBool a -> expr a
    I.CondDeref _ ref v a -> do
      expr ref >>= lookupState >>= extendEnv (var v)
      cond a

-- Rewrite statement blocks.
block :: I.Block -> V I.Block
block = mapM stmt

-- Defines a new verification condition (VC).
newVC :: I.Expr -> V Expr
newVC check = do
  check <- expr check
  m <- get
  let vc = "vc" ++ show (nextVCId m)
  set m { nextVCId = nextVCId m + 1, body = body m . let' vc (implies (branch m) check) }
  return $ Var vc

-- Adds a VC as a lemma.
addVC :: Expr -> V ()
addVC a = do
  m <- get
  set m { lemmas = lemmas m ++ [a] }

-- Checks a verification condition then adds it to the list of lemmas.
checkVC :: Expr -> V Bool
checkVC a = do
  m <- get
  let thm = body m $ implies (foldl and' true $ lemmas m) a
  lift $ print thm
  lift $ putStrLn ""
  pass <- lift $ A.check [A.call "set-ignore-ok" [A.t], A.thm $ acl2 thm]
  --if not pass then lift (print thm) else return ()
  addVC a
  return pass

-- Name of current procedure undergoing analysis.
proc :: V String
proc = do
  m <- get
  return $ procName m

-- Defines a branching condition (BC).
newBC :: I.Expr -> V Expr
newBC cond = do
  cond <- expr cond
  m <- get
  let bc = "bc" ++ show (nextBCId m)
  set m { nextBCId = nextBCId m + 1, body = body m . let' bc cond }
  return $ Var bc

-- Create a new free variable.
newFree :: V Expr
newFree = do
  m <- get
  let free = "free" ++ show (nextFreeId m)
  set m { nextFreeId = nextFreeId m + 1 }
  addBody $ ForAll [free]
  return $ Var free

-- Create a new env variable.
newEnv :: V String
newEnv = do
  m <- get
  set m { nextEnvId = nextEnvId m + 1 }
  return $ "env" ++ show (nextEnvId m)

addBody :: (Expr -> Expr) -> V ()
addBody b = do
  m <- get
  set m { body = body m . b }

getEnv :: V Expr
getEnv = do
  m <- get
  return $ env m

setEnv :: Expr -> V ()
setEnv a = do
  m <- get
  set m { env = a }

extendEnv :: String -> Expr -> V ()
extendEnv a b = do
  env0 <- getEnv
  env1 <- newEnv
  addBody $ let' env1 $ RecordCons a b env0
  setEnv $ Var env1

lookupEnv :: String -> V Expr
lookupEnv a = do
  env <- getEnv
  return $ RecordProj env a

-- Create a new state variable and sets it as the current state.
newState :: (Expr -> Expr) -> V ()
newState f = do
  m <- get
  let state0 = state m
      state1 = "state" ++ show (nextStateId m)
  set m
    { nextStateId = nextStateId m + 1
    , state = Var state1
    , body = body m . let' state1 (f state0)
    }

getState :: V Expr
getState = do
  m <- get
  return $ state m

-- Extends the state, returns pointer (index) to added state.
extendState :: Expr -> V Expr
extendState a = do
  state0 <- getState
  newState $ \ state0 -> ArrayCons state0 a
  return $ length' state0

-- Updates a state value.
updateState :: Expr -> Expr -> V ()
updateState ref value = newState $ ArrayUpdate ref value

-- Dereference a reference.
lookupState :: Expr -> V Expr
lookupState ref = do
  state <- getState
  return $ ArrayProj state ref

-- Rewrite statements.
stmt :: I.Stmt -> V I.Stmt
stmt a = case a of
  I.Assert         b -> checkAssert a b
  I.CompilerAssert b -> checkAssert a b
  I.Assume         b -> checkAssert a b

  I.IfTE a b c -> do
    cond <- newBC a
    m0 <- get
    set m0 { branch = and' (branch m0) cond }
    b <- block b
    m1 <- get
    set m1 { branch = and' (branch m0) $ not' cond, lemmas = lemmas m0, env = env m0, state = state m0 }
    c <- block c
    m2 <- get
    let l = length $ lemmas m0
        lemmas1 = drop l $ lemmas m1
        lemmas2 = drop l $ lemmas m2
    set m2
      { branch = branch m0
      , lemmas = lemmas m0 ++ lemmas1 ++ lemmas2
      , env    = env m0
      }
    newState $ const $ if' cond (state m1) (state m2)
    return $ I.IfTE a b c

  I.Deref    _ v r -> expr r >>= lookupState >>= extendEnv (var v) >> return a

  I.AllocRef _ r v -> lookupEnv (var v) >>= extendState >>= extendEnv (var r) >> return a

  I.Store    _ r v -> do
    r <- expr r
    v <- expr v
    updateState r v
    return a

  I.RefCopy _ r1 r2  -> do
    r1 <- expr r1
    r2 <- expr r2
    lookupState r2 >>= updateState r1
    return a

  -- XXX
  I.Assign _ _ _   -> return a
  I.Return _       -> return a
  I.ReturnVoid     -> return a
  I.Local _ _ _    -> return a
  I.Call  _ _ _ _  -> return a
  I.Forever _      -> return a
  I.Loop _ _ _ _   -> return a
  I.Break          -> return a
  I.Comment _      -> return a
  

-- Verified assertions are turned into comments.
checkAssert :: I.Stmt -> I.Expr -> V I.Stmt
checkAssert stmt check = do
  proc <- proc
  vc <- newVC check
  pass <- checkVC vc
  if pass
    then do
      return $ I.Comment $ "Assertion verified: " ++ show stmt
    else do
      lift $ putStrLn $ "Assertion failed in " ++ proc ++ ": " ++ show stmt
      return stmt

-- Convert an Ivory boolean expression to ACL2.
--bool :: I.Expr -> V Expr
--bool = undefined -- a = expr a >>= return . A.not' . A.zip'

-- Convert an Ivory expression.  Unsupported expressions are converted to free variables (ForAll a).
expr :: I.Expr -> V Expr
expr a = case a of
  I.ExpSym      a       -> return $ Var a
  I.ExpVar      a       -> return $ Var $ var a
  I.ExpLit      a       -> case a of
    I.LitBool    a -> return $ Bool    a
    I.LitInteger a -> return $ Integer a
    _ -> newFree
  I.ExpIndex    _ a _ b -> do { a <- expr a >>= lookupState; b <- expr b; return $ ArrayProj a b }
  I.ExpLabel    _ a b   -> do { a <- expr a >>= lookupState; return $ RecordProj a b }
  I.ExpToIx     a _     -> expr a   -- Is it ok to ignore the maximum bound?
  I.ExpSafeCast _ a     -> expr a
  I.ExpOp       op args -> do
    args <- mapM expr args
    intrinsic op args
  _ -> newFree

-- Convert Ivory intrinsitcs.
intrinsic :: I.ExpOp -> [Expr] -> V Expr
intrinsic op args = case op of
  {-
  I.ExpEq   _        -> Just M.Eq
  I.ExpNeq  _        -> Just M.Neq
  I.ExpCond          -> Just M.Cond  
  I.ExpGt   False _  -> Just M.Gt
  I.ExpGt   True  _  -> Just M.Ge
  I.ExpLt   False _  -> Just M.Lt
  I.ExpLt   True  _  -> Just M.Le
  -}
  I.ExpNot           -> return $ not' (args !! 0)
  I.ExpAnd           -> return $ and' (args !! 0) (args !! 1)
  I.ExpOr            -> return $ or'  (args !! 0) (args !! 1)
  {-
  I.ExpMul           -> Just M.Mul   
  I.ExpMod           -> Just M.Mod   
  I.ExpAdd           -> Just M.Add   
  I.ExpSub           -> Just M.Sub   
  I.ExpNegate        -> Just M.Negate
  I.ExpAbs           -> Just M.Abs   
  I.ExpSignum        -> Just M.Signum
  -}
  _                  -> newFree

acl2 :: Expr -> A.Expr
acl2 = const A.t  -- XXX

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

data UniOp = Not | Length deriving Eq
data BinOp = And | Or | Implies deriving Eq

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

instance Show BinOp where
  show a = case a of
    And -> "&&"
    Or  -> "||"
    Implies -> "->"

let' = Let

--not' = UniOp Not
not' a = case a of
  Bool a -> Bool $ not a
  a      -> UniOp Not a

--and' = BinOp And
and' a b = case (a, b) of
  (Bool a, Bool b) -> Bool $ a && b
  (Bool False, _)  -> false
  (_, Bool False)  -> false
  (Bool True, b)   -> b
  (a, Bool True)   -> a
  (a, b)
    | a == b       -> a
    | otherwise    -> BinOp And a b

--or' = BinOp Or
or' a b = case (a, b) of
  (Bool a, Bool b) -> Bool $ a || b
  (Bool True, _)   -> true
  (_, Bool True)   -> true
  (Bool False, b)  -> b
  (a, Bool False)  -> a
  (a, b)
    | a == b       -> a
    | otherwise    -> BinOp Or a b

--implies = BinOp Implies
implies a b = case (a, b) of
  (Bool a, b)      -> not' (Bool a) `or'` b
  (a, b)
    | a == b       -> true
    | otherwise    -> BinOp Implies a b

true  = Bool True
false = Bool False
if' = If
length' = UniOp Length

