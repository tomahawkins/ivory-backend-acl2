-- | Verifies and removes assertions from an Ivory program.
module Ivory.Opts.Asserts
  ( assertsFold
  ) where

import Data.List
import Data.Maybe
import MonadLib
import Text.Printf

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
      (p, _) <- runStateT init $ comment' (printf "Procedure: %s(%s)" (I.procSym proc) $ intercalate ", " [ var $ I.tValue a | a <- I.procArgs proc ]) $ do
        comment' "Procedure arguments and initial environment." $ do
          argVars <- mapM (const newFree) args
          newEnv $ Record [ (a, b) | (a, b) <- zip args argVars ]
        comment' "Assuming requires." $ mapM_ assumeRequire $ I.procRequires proc
        body <- comment' "Procedure body. " $ block $ I.procBody proc
        ensures <- comment' "Checking ensures." $ mapM checkEnsure (I.procEnsures proc) >>= return . catMaybes
        return $ proc { I.procBody = body, I.procEnsures = ensures }
      return p
      where
      args = map (var . I.tValue) $ I.procArgs proc
      init = VDB
        { nextVCId     = 0
        , nextBCId     = 0
        , nextFreeId   = 0
        , nextEnvId    = 0
        , nextAssumeId = 0
        , nextStackId  = 1
        , procName'    = I.procSym proc
        , procs        = concat [ I.public (I.modProcs m) ++ I.private (I.modProcs m) | m <- modules ]
        , body         = forAll ["stack0"]
        , branch       = true
        , lemmas       = []
        , env          = undefined
        , stack        = Var "stack0"
        , returns      = []
        }

-- Data carried through verification monad.
data VDB = VDB
  { nextVCId     :: Int
  , nextBCId     :: Int
  , nextFreeId   :: Int
  , nextEnvId    :: Int
  , nextStackId  :: Int
  , nextAssumeId :: Int
  , procName'    :: String
  , procs        :: [I.Proc]
  , body         :: Expr -> Expr
  , branch       :: Expr
  , lemmas       :: [Expr]
  , env          :: Expr
  , stack        :: Expr
  , returns      :: [Expr]
  }

-- Verification monad.
type V a = StateT VDB IO a

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

-- Insert a comment.
comment :: String -> V ()
comment a = addBody $ Comment a

-- Insert a nested comment.
comment' :: String -> V a -> V a
comment' a b = do
  comment $ "( " ++ a
  b <- b
  comment ")"
  return b

-- Create a new environment variable.
newEnv :: Expr -> V ()
newEnv a = do
  m <- get
  set m { nextEnvId = nextEnvId m + 1 }
  let env = "env" ++ show (nextEnvId m)
  addBody $ let' env a
  setEnv $ Var env

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

-- Restores the current environment on completion.
withEnv :: V a -> V a
withEnv a = do
  env0 <- getEnv
  a <- a
  setEnv env0
  return a

-- Extend the current environment with a name and a value.
extendEnv :: String -> Expr -> V ()
extendEnv a b = do
  env0 <- getEnv
  newEnv $ RecordOverlay (Record [(a, b)]) env0

-- Lookup a name in the current environment.
lookupEnv :: String -> V Expr
lookupEnv a = do
  env <- getEnv
  return $ RecordProject env a

-- Create a new stack variable and sets it as the current stack.
newStack :: (Expr -> Expr) -> V ()
newStack f = do
  m <- get
  let stack0 = stack m
      stack1 = "stack" ++ show (nextStackId m)
  set m
    { nextStackId = nextStackId m + 1
    , stack = Var stack1
    , body = body m . let' stack1 (f stack0)
    }

-- Get the current stack.
getStack :: V Expr
getStack = do
  m <- get
  return $ stack m

-- Extends the stack, returns pointer (index) to added stack.
extendStack :: Expr -> V Expr
extendStack a = do
  stack0 <- getStack
  newStack $ \ stack0 -> ArrayAppend stack0 $ Array [a]
  return $ length' stack0

-- Updates a stack value.
updateStack :: Expr -> Expr -> V ()
updateStack ref value = newStack $ ArrayUpdate ref value

-- Dereference a reference on the stack.
lookupStack :: Expr -> V Expr
lookupStack ref = do
  stack <- getStack
  return $ ArrayProject stack ref

-- Create a new stack of free variables, but keeps the stack size the same.
freeStack :: V ()
freeStack = comment' "Free the stack, but keep the number of elements the same." $ do
  m <- get
  let stack0 = stack m
      stack1 = "stack" ++ show (nextStackId m)
  set m
    { nextStackId = nextStackId m + 1
    , stack = Var stack1
    , body = body m . forAll [stack1]
    }
  addLemma $ length' stack0 ==. length' (Var stack1)
  

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
assumeRequire (I.Require a) = condition Nothing a >>= addLemma

-- Check ensures on all return points and remove if possible.
checkEnsure :: I.Ensure -> V (Maybe I.Ensure)
checkEnsure ensure@(I.Ensure a) = do
  pass <- getReturns >>= mapM (flip condition a . Just) >>= mapM checkEnsureReturn >>= return . and
  if pass then return $ Just ensure else return Nothing
  where
  -- Check an single return point.
  checkEnsureReturn :: Expr -> V Bool
  checkEnsureReturn check = do
    proc <- procName
    lift $ printf "Checking ensure at return point:  procedure = %s  ensure = %s\n" proc (show check)
    pass <- checkVC check
    if pass then return () else lift $ putStrLn $ "Ensure failed in " ++ proc ++ ": " ++ show ensure
    return pass

-- Check a sub require condition on a function call.
checkSubRequire :: String -> I.Require -> V Bool
checkSubRequire callee req@(I.Require a) = do
  check <- condition Nothing a
  caller <- procName
  lift $ printf "Checking sub-require:  caller = %s  callee = %s  require = %s\n"  caller callee (show check)
  pass <- checkVC check
  if pass then return () else lift $ putStrLn $ "Require failed in " ++ callee ++ " when called from " ++ caller ++ ": " ++ show req
  return pass

-- Assume a sub ensure condition when a function call returns.
assumeSubEnsure :: Expr -> I.Ensure -> V ()
assumeSubEnsure ret (I.Ensure a) = condition (Just ret) a >>= addLemma

-- Verified assertions are turned into comments.
checkAssert :: I.Stmt -> I.Expr -> V I.Stmt
checkAssert stmt check = do
  proc <- procName
  check <- expr check
  lift $ printf "Checking assert:  procedure = %s  assert = %s\n" proc (show check)
  pass <- checkVC check
  if pass
    then do
      return $ I.Comment $ "Assertion verified: " ++ show stmt
    else do
      lift $ putStrLn $ "Assertion failed in " ++ proc ++ ": " ++ show stmt
      return stmt

-- Checks a verification condition then adds it to the list of lemmas.
checkVC :: Expr -> V Bool
checkVC a = do
  a <- newVC a
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
  m <- get
  set m { lemmas = lemmas m ++ [a] }
  return pass

-- Defines a new verification condition (VC).
newVC :: Expr -> V Expr
newVC check = do
  m <- get
  let vc = "vc" ++ show (nextVCId m)
      vc' = implies (branch m) check
  set m { nextVCId = nextVCId m + 1 }
  addBody $ let' vc vc'
  return $ Var vc

-- Adds a VC as a lemma.
addLemma :: Expr -> V ()
addLemma a = do
  m <- get
  let assume = "assume" ++ show (nextAssumeId m)
      assume' = implies (branch m ) a
  set m { nextAssumeId = nextAssumeId m + 1 }
  addBody $ let' assume assume'
  m <- get
  set m { lemmas = lemmas m ++ [Var assume] }

-- Converts a condition to an expression.
condition :: Maybe Expr -> I.Cond -> V Expr
condition retval a = case a of
  I.CondBool a -> expr' retval a
  I.CondDeref _ ref v a -> do
    expr ref >>= lookupStack >>= extendEnv (var v)
    condition retval a

-- Rewrite statement blocks.
block :: I.Block -> V I.Block
block = mapM stmt

-- Rewrite statements.
stmt :: I.Stmt -> V I.Stmt
stmt a = case a of
  I.Comment        c -> comment ("Ivory comment: " ++ c) >> return a
  I.Assert         b -> checkAssert a b
  I.CompilerAssert b -> checkAssert a b
  I.Assume         b -> checkAssert a b

  I.IfTE a b c -> do
    comment' "If statement." $ do
      cond <- newBC a
      m0 <- get
      set m0 { branch = branch m0 &&. cond }
      b <- comment' "If condition == true." $ block b
      m1 <- get
      set m1 { branch = branch m0 &&. not' cond, lemmas = lemmas m0, env = env m0, stack = stack m0 }
      c <- comment' "If condition == false." $ block c
      m2 <- get
      let l = length $ lemmas m0
          lemmas1 = drop l $ lemmas m1
          lemmas2 = drop l $ lemmas m2
      set m2
        { branch = branch m0
        , lemmas = lemmas m0 ++ lemmas1 ++ lemmas2
        , env    = env m0
        }
      newStack $ const $ if' cond (stack m1) (stack m2)
      return $ I.IfTE a b c

  -- How do we tell the difference between derefence from the stack or a global memory area?
  I.Deref    _ v r -> comment' "Dereference a reference." $ expr r >>= lookupStack >>= extendEnv (var v) >> return a

  I.AllocRef _ r v -> comment' "Allocate a reference." $ lookupEnv (var v) >>= extendStack >>= extendEnv (var r) >> return a

  I.Store    _ r v -> comment' "Store a value to a reference." $ do
    r <- expr r
    v <- expr v
    updateStack r v
    return a

  I.RefCopy _ r1 r2  -> comment' "Copy a reference." $ do
    r1 <- expr r1
    r2 <- expr r2
    lookupStack r2 >>= updateStack r1
    return a

  I.Return b       -> comment' "Return a value." $ expr (I.tValue b) >>= addReturn >> return a
  I.ReturnVoid     -> comment' "Return void."    $ addReturn unit >> return a

  I.Local t v i    -> comment' "Local variable introduction." $ do
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
    p <- getProc $ var f
    comment' (printf "Calling procedure:  %s(%s)" (var f) $ intercalate ", " [ var $ I.tValue a | a <- I.procArgs p ]) $ do
      returnValue <- withEnv $ do
        comment' "Binding input arguments to new environment." $ do
          args <- mapM (expr . I.tValue) args
          newEnv $ Record [ (var $ I.tValue a, b) | (a, b) <- zip (I.procArgs p) args ] 
        comment' "Checking sub-requires." $ mapM_ (checkSubRequire $ var f) $ I.procRequires p  -- XXX Need to collect results for all call sites to be able to remove requires.
        freeStack
        returnValue <- comment' "Return value." newFree
        comment' "Assuming sub-ensures." $ mapM_ (assumeSubEnsure returnValue) $ I.procEnsures p
        return returnValue
      case ret of
        Nothing -> return ()
        Just ret -> comment' "Extend environment with return value." $ extendEnv (var ret) returnValue
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
expr = expr' Nothing

-- Convert an Ivory expression, replacing 'retval' varaibles with an optional expression.
expr' :: Maybe Expr -> I.Expr -> V Expr
expr' retval a = case a of
  I.ExpSym      a       -> lookupEnv a
  I.ExpVar      a       -> case retval of
    Just b | var a == "retval" -> return b
    _ -> lookupEnv $ var a
  I.ExpLit      a       -> case a of
    I.LitBool    a -> return $ Bool    a
    I.LitInteger a -> return $ Integer a
    _ -> newFree
  I.ExpIndex    _ a _ b -> do { a <- expr a >>= lookupStack; b <- expr b; return $ ArrayProject a b }
  I.ExpLabel    _ a b   -> do { a <- expr a >>= lookupStack; return $ RecordProject a b }
  I.ExpToIx     a _     -> expr a   -- Is it ok to ignore the maximum bound?
  I.ExpSafeCast _ a     -> expr a
  I.ExpOp       op args -> do
    args <- mapM expr args
    intrinsic op args
  _ -> newFree
  where
  expr = expr' retval


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

