-- | Verifies and removes assertions from an Ivory program.
module Ivory.Opts.Asserts
  ( assertsFold
  ) where

import Data.Maybe
import MonadLib

import qualified Ivory.Language.Syntax.AST  as I
import qualified Ivory.Language.Syntax.Type as I
import Ivory.Compile.ACL2 (var)
import qualified Mira.ACL2 as A
import Mira.VC

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
      (p, _) <- runStateT init $ do
        mapM_ assumeRequire $ I.procRequires proc
        body <- block $ I.procBody proc
        ensures <- mapM checkEnsure (I.procEnsures proc) >>= return . catMaybes
        return $ proc { I.procBody = body, I.procEnsures = ensures }
      return p
      where
      args = map (var . I.tValue) $ I.procArgs proc
      init = VDB
        { nextVCId    = 0
        , nextBCId    = 0
        , nextFreeId  = 0
        , nextEnvId   = 1
        , nextStateId = 1
        , procName'   = I.procSym proc
        , procs       = concat [ I.public (I.modProcs m) ++ I.private (I.modProcs m) | m <- modules ]
        , body        = forAll ("state0" : args) . let' "env0" (Record [ (a, Var a) | a <- args])
        , branch      = true
        , lemmas      = []
        , env         = Var "env0"
        , state       = Var "state0"
        , returns     = []
        }

-- Data carried through verification monad.
data VDB = VDB
  { nextVCId    :: Int
  , nextBCId    :: Int
  , nextFreeId  :: Int
  , nextEnvId   :: Int
  , nextStateId :: Int
  , procName'   :: String
  , procs       :: [I.Proc]
  , body        :: Expr -> Expr
  , branch      :: Expr
  , lemmas      :: [Expr]
  , env         :: Expr
  , state       :: Expr
  , returns     :: [Expr]
  }

-- Verification monad.
type V a = StateT VDB IO a

-- Defines a new verification condition (VC).
newVC :: Expr -> V Expr
newVC check = do
  m <- get
  let vc = "vc" ++ show (nextVCId m)
      vc' = implies (branch m) check
  case vc' of
    Bool a -> return $ Bool a
    _ -> do 
      set m { nextVCId = nextVCId m + 1, body = body m . let' vc vc' }
      return $ Var vc

-- Adds a VC as a lemma.
addLemma :: Expr -> V ()
addLemma a = do
  m <- get
  set m { lemmas = lemmas m ++ [a] }

-- Name of current procedure undergoing analysis.
procName :: V String
procName = do
  m <- get
  return $ procName' m

-- Defines a branching condition (BC).
newBC :: I.Expr -> V Expr
newBC cond = do
  cond <- expr cond
  m <- get
  let bc = "bc" ++ show (nextBCId m)
  set m { nextBCId = nextBCId m + 1, body = body m . let' bc cond }
  return $ Var bc

-- Create a new free variable.  Used to model unmodelable Ivory constructs.
newFree :: V Expr
newFree = do
  m <- get
  let free = "free" ++ show (nextFreeId m)
  set m { nextFreeId = nextFreeId m + 1 }
  addBody $ forAll [free]
  return $ Var free

-- Add some new procedure body logic.
addBody :: (Expr -> Expr) -> V ()
addBody b = do
  m <- get
  set m { body = body m . b }

-- Create a new environment variable.
newEnv :: V String
newEnv = do
  m <- get
  set m { nextEnvId = nextEnvId m + 1 }
  return $ "env" ++ show (nextEnvId m)

-- Get the current environment.
getEnv :: V Expr
getEnv = do
  m <- get
  return $ env m

-- Set the current environment.
setEnv :: Expr -> V ()
setEnv a = do
  m <- get
  set m { env = a }

-- Extend the current environment with a name and a value.
extendEnv :: String -> Expr -> V ()
extendEnv a b = do
  env0 <- getEnv
  env1 <- newEnv
  addBody $ let' env1 $ RecordOverlay (Record [(a, b)]) env0
  setEnv $ Var env1

-- Lookup a name in the current environment.
lookupEnv :: String -> V Expr
lookupEnv a = do
  env <- getEnv
  return $ RecordProject env a

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

-- Get the current state.
getState :: V Expr
getState = do
  m <- get
  return $ state m

-- Extends the state, returns pointer (index) to added state.
extendState :: Expr -> V Expr
extendState a = do
  state0 <- getState
  newState $ \ state0 -> ArrayAppend state0 $ Array [a]
  return $ length' state0

-- Updates a state value.
updateState :: Expr -> Expr -> V ()
updateState ref value = newState $ ArrayUpdate ref value

-- Dereference a reference.
lookupState :: Expr -> V Expr
lookupState ref = do
  state <- getState
  return $ ArrayProject state ref

-- Add a return expression.
addReturn :: Expr -> V ()
addReturn a = do
  m <- get
  set m { returns = returns m ++ [a] }

-- Get all the return expressions in the procedure.
getReturns :: V [Expr]
getReturns = do
  m <- get
  return $ returns m

-- Get a procedure by name.
getProc :: String -> V I.Proc
getProc f = do
  m <- get
  case [ p | p <- procs m, I.procSym p == f ] of
    [p] -> return p
    []  -> error $ "Procedure not found: " ++ f
    _   -> error $ "Multple procedures found: " ++ f

-- Convert requires to lemmas.
assumeRequire :: I.Require -> V ()
assumeRequire (I.Require a) = condition a >>= addLemma

-- Check ensures on all return points and remove if possible.
checkEnsure :: I.Ensure -> V (Maybe I.Ensure)
checkEnsure ensure@(I.Ensure a) = do
  cond <- condition a
  pass <- getReturns >>= mapM (checkEnsureReturn . retval cond) >>= return . and
  if pass then return $ Just ensure else return Nothing
  where
  -- Check an single return point.
  checkEnsureReturn :: Expr -> V Bool
  checkEnsureReturn check = do
    proc <- procName
    vc <- newVC check
    pass <- checkVC vc
    if pass then return () else lift $ putStrLn $ "Ensure failed in " ++ proc ++ ": " ++ show ensure
    return pass

-- Check a sub require condition on a function call.
checkSubRequire :: String -> I.Require -> V Bool
checkSubRequire callee req@(I.Require a) = do
  cond <- condition a
  caller <- procName
  vc <- newVC cond
  pass <- checkVC vc
  if pass then return () else lift $ putStrLn $ "Require failed in " ++ callee ++ " when called from " ++ caller ++ ": " ++ show req
  return pass

-- Assume a sub ensure condition when a function call returns.
assumeSubEnsure :: Expr -> I.Ensure -> V ()
assumeSubEnsure ret (I.Ensure a) = do
  cond <- condition a
  addLemma $ retval cond ret

-- Verified assertions are turned into comments.
checkAssert :: I.Stmt -> I.Expr -> V I.Stmt
checkAssert stmt check = do
  proc <- procName
  check <- expr check
  vc <- newVC check
  pass <- checkVC vc
  if pass
    then do
      return $ I.Comment $ "Assertion verified: " ++ show stmt
    else do
      lift $ putStrLn $ "Assertion failed in " ++ proc ++ ": " ++ show stmt
      return stmt

-- Checks a verification condition then adds it to the list of lemmas.
checkVC :: Expr -> V Bool
checkVC a = do
  m <- get
  let thm = body m $ implies (foldl (&&.) true $ lemmas m) a
      thm' = optimize thm
  lift $ putStrLn "VC:"
  lift $ print thm
  lift $ putStrLn "\nOptimized VC:"
  lift $ print thm'
  lift $ putStrLn ""
  pass <- case thm' of
    -- Don't call ACL2 if the result is obvious.
    Bool a -> return a
    --thm -> lift $ A.check [A.call "set-ignore-ok" [A.t], A.thm $ acl2 thm']
    thm -> lift $ A.check [A.thm $ acl2 thm]
  addLemma a
  return pass

-- Replace the 'retval' expressions with a return expression.
retval :: Expr -> Expr -> Expr
retval a ret = case a of
  Var "retval" -> ret
  Var a        -> Var a
  Let a b c
    | a == "retval" -> Let a b c
    | otherwise     -> Let a (retval' b) (retval' c)
  ForAll a b
    | a == "retval" -> ForAll a b
    | otherwise     -> ForAll a $ retval' b
  Record        a     -> Record [ (a, retval' b) | (a, b) <- a ]
  RecordOverlay a b   -> RecordOverlay (retval' a) (retval' b)
  RecordProject a b   -> RecordProject (retval' a) b
  Array         a     -> Array $ map retval' a
  ArrayAppend   a b   -> ArrayAppend (retval' a) (retval' b)
  ArrayUpdate   a b c -> ArrayUpdate (retval' a) (retval' b) (retval' c)
  ArrayProject  a b   -> ArrayProject (retval' a) (retval' b)
  Unit -> Unit
  Bool a -> Bool a
  Integer a -> Integer a
  UniOp a b -> UniOp a $ retval' b
  BinOp a b c -> BinOp a (retval' b) (retval' c)
  If a b c -> If (retval' a) (retval' b) (retval' c)
  where
  retval' = flip retval ret

-- Converts a condition to an expression.
condition :: I.Cond -> V Expr
condition a = case a of
  I.CondBool a -> expr a
  I.CondDeref _ ref v a -> do
    expr ref >>= lookupState >>= extendEnv (var v)
    condition a

-- Rewrite statement blocks.
block :: I.Block -> V I.Block
block = mapM stmt

-- Rewrite statements.
stmt :: I.Stmt -> V I.Stmt
stmt a = case a of
  I.Comment        _ -> return a
  I.Assert         b -> checkAssert a b
  I.CompilerAssert b -> checkAssert a b
  I.Assume         b -> checkAssert a b

  I.IfTE a b c -> do
    cond <- newBC a
    m0 <- get
    set m0 { branch = branch m0 &&. cond }
    b <- block b
    m1 <- get
    set m1 { branch = branch m0 &&. not' cond, lemmas = lemmas m0, env = env m0, state = state m0 }
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

  I.Return b       -> expr (I.tValue b) >>= addReturn >> return a
  I.ReturnVoid     -> addReturn unit >> return a

  I.Local t v i    -> do
    init i >>= extendEnv (var v)
    return a
    where
    init :: I.Init -> V Expr
    init a = case (t, a) of
      (I.TyBool,   I.InitZero) -> return $ Bool False
      (I.TyInt _,  I.InitZero) -> return $ Integer 0
      (I.TyWord _, I.InitZero) -> return $ Integer 0
      (_, I.InitExpr _ a) -> expr a
      (_, I.InitStruct a) -> do { b <- mapM init $ snd $ unzip a; return $ Record $ zip (fst $ unzip a) b }
      (_, I.InitArray  a) -> do { a <- mapM init a; return $ Array a }
      _ -> newFree

  I.Assign _ v b   -> expr b >>= extendEnv (var v) >> return a

  I.Call  _ ret f args  -> do
    -- Check sub-requires.
    env0 <- getEnv
    args <- mapM (expr . I.tValue) args
    p <- getProc (var f)
    env1 <- newEnv
    addBody $ let' env1 $ Record [ (var $ I.tValue a, b) | (a, b) <- zip (I.procArgs p) args ] 
    setEnv $ Var env1
    mapM_ (checkSubRequire $ var f) $ I.procRequires p  -- XXX Need to collect results for all call sites to be able to remove requires.
    setEnv env0

    -- Assume sub-ensures.
    case ret of
      Nothing -> mapM_ (assumeSubEnsure unit) $ I.procEnsures p
      Just ret -> do
        free <- newFree
        extendEnv (var ret) free
        mapM_ (assumeSubEnsure $ Var $ var ret) $ I.procEnsures p
    
    return a

  -- XXX
  _ -> error $ "Unsupported stmt: " ++ show a
  {-
  I.Loop _ _ _ _   -> return a
  I.Forever _      -> return a
  I.Break          -> return a
  -}

-- Convert an Ivory expression.  Unsupported expressions are converted to free variables (ForAll a).
expr :: I.Expr -> V Expr
expr a = case a of
  I.ExpSym      a       -> lookupEnv a
  I.ExpVar      a       -> lookupEnv $ var a
  I.ExpLit      a       -> case a of
    I.LitBool    a -> return $ Bool    a
    I.LitInteger a -> return $ Integer a
    _ -> newFree
  I.ExpIndex    _ a _ b -> do { a <- expr a >>= lookupState; b <- expr b; return $ ArrayProject a b }
  I.ExpLabel    _ a b   -> do { a <- expr a >>= lookupState; return $ RecordProject a b }
  I.ExpToIx     a _     -> expr a   -- Is it ok to ignore the maximum bound?
  I.ExpSafeCast _ a     -> expr a
  I.ExpOp       op args -> do
    args <- mapM expr args
    intrinsic op args
  _ -> newFree

-- Convert Ivory intrinsitcs.
intrinsic :: I.ExpOp -> [Expr] -> V Expr
intrinsic op args = case op of
  I.ExpEq   _        -> return $ arg 0 ==. arg 1
  I.ExpNeq  _        -> return $ not' (arg 0 ==. arg 1)
  I.ExpCond          -> return $ if' (arg 0) (arg 1) (arg 2)
  I.ExpGt   False _  -> return $ arg 0 >.  arg 1
  I.ExpGt   True  _  -> return $ arg 0 >=. arg 1
  I.ExpLt   False _  -> return $ arg 0 <.  arg 1
  I.ExpLt   True  _  -> return $ arg 0 <=. arg 1
  I.ExpNot           -> return $ not' (arg 0)
  I.ExpAnd           -> return $ arg 0 &&. arg 1
  I.ExpOr            -> return $ arg 0 ||. arg 1
  I.ExpAdd           -> return $ arg 0 + arg 1
  I.ExpSub           -> return $ arg 0 - arg 1
  I.ExpMul           -> return $ arg 0 * arg 1
  I.ExpMod           -> return $ mod' (arg 0) (arg 1)
  I.ExpNegate        -> return $ negate (arg 0)
  I.ExpAbs           -> return $ abs    (arg 0)
  I.ExpSignum        -> return $ signum (arg 0)
  _                  -> newFree
  where
  arg = (args !!)

acl2 :: Expr -> A.Expr
acl2 = const A.t  -- XXX

