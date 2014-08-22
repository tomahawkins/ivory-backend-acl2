-- | Verifies and removes assertions from an Ivory program.
module Ivory.Opts.Asserts
  ( assertsFold
  ) where

import MonadLib

import qualified Ivory.Language.Syntax.AST as I
import Ivory.Compile.ACL2 (var, lit)
import qualified Mira.Expr  as M
import qualified Mira.ACL2 as A

-- Data carried through verification monad.
data VDB = VDB
  { nextVCId    :: Int
  , nextBCId    :: Int
  , nextFreeId  :: Int
  , procName    :: String
  , body        :: A.Expr -> A.Expr
  , branch      :: A.Expr
  , lemmas      :: A.Expr
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
      b <- runStateT init $ block $ I.procBody proc
      return $ proc { I.procBody = fst b }
      where
      init = VDB
        { nextVCId = 0
        , nextBCId = 0
        , nextFreeId = 0
        , procName = I.procSym proc
        , body     = id
        , branch   = A.t
        , lemmas   = A.t
        }

block :: I.Block -> V I.Block
block = mapM stmt

-- Defines a new verification condition (VC).
newVC :: I.Expr -> V A.Expr
newVC check = do
  check <- bool check
  m <- get
  let vc = "_vc" ++ show (nextVCId m)
  set m { nextVCId = nextVCId m + 1, body = body m . A.let' [(vc, A.implies (branch m) check)] }
  return $ A.var vc

-- Adds a VC as a lemma.
addVC :: A.Expr -> V ()
addVC a = do
  m <- get
  set m { lemmas = A.and' (lemmas m) a }

-- Checks a verification condition then adds it to the list of lemmas.
checkVC :: A.Expr -> V Bool
checkVC a = do
  m <- get
  pass <- lift $ A.check [A.thm $ body m $ A.implies (lemmas m) a]
  addVC a
  return pass

-- Name of current procedure undergoing analysis.
proc :: V String
proc = do
  m <- get
  return $ procName m

-- Defines a branching condition (BC).
newBC :: I.Expr -> V A.Expr
newBC cond = do
  cond <- bool cond
  m <- get
  let bc = "_bc" ++ show (nextBCId m)
  set m { nextBCId = nextBCId m + 1, body = body m . A.let' [(bc, cond)] }
  return $ A.var bc

-- Create a new free variable.
newFree :: V String
newFree = do
  m <- get
  set m { nextFreeId = nextFreeId m + 1 }
  return $ "_free" ++ show (nextFreeId m)

stmt :: I.Stmt -> V I.Stmt
stmt a = case a of
  I.Assert         b -> checkAssert a b
  I.CompilerAssert b -> checkAssert a b
  I.Assume         b -> checkAssert a b

  -- XXX How to handle state and env changes?  Need to hide env changes after the branch merges.
  I.IfTE a b c -> do
    cond <- newBC a
    m0 <- get
    set m0 { branch = A.and' (branch m0) cond }
    b <- block b
    m1 <- get
    set m1 { branch = A.and' (branch m0) $ A.not' cond }   -- Asserts in true branch are used as lemmas is false branch, but I don't think they help.
    c <- block c
    m2 <- get
    set m2 { branch = branch m0 }
    return $ I.IfTE a b c

  I.Deref  _ _ _   -> return a
  I.Store  _ _ _   -> return a
  I.Assign _ _ _   -> return a
  I.Return _       -> return a
  I.ReturnVoid     -> return a
  I.Local _ _ _    -> return a
  I.Call  _ _ _ _  -> return a
  I.RefCopy _ _ _  -> return a
  I.AllocRef _ _ _ -> return a
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

-- Convert an Ivory expression to ACL2.
expr :: I.Expr -> V A.Expr
expr a = expr' a >>= return . M.exprACL2

-- Convert an Ivory boolean expression to ACL2.
bool :: I.Expr -> V A.Expr
bool a = expr a >>= return . A.not' . A.zip'

-- Convert an Ivory expression to a CLL expression to reuse the CLL-to-ACL2 infrastructure.
-- Unsupported expressions are converted to free variables.
expr' :: I.Expr -> V M.Expr
expr' a = case a of
  I.ExpSym      a       -> return $ M.Var a
  I.ExpVar      a       -> return $ M.Var $ var a
  I.ExpLit      a       -> return $ M.Literal $ lit a
  I.ExpIndex    _ a _ b -> do { a <- expr' a; b <- expr' b; return $ M.ArrayIndex  (M.Deref a) b }
  I.ExpLabel    _ a b   -> do { a <- expr' a;               return $ M.StructIndex (M.Deref a) b }
  I.ExpToIx     a _     -> expr' a   -- Is it ok to ignore the maximum bound?
  I.ExpSafeCast _ a     -> expr' a
  I.ExpOp       op args -> case intrinsic op of
    Just i  -> do { args <- mapM expr' args; return $ M.Intrinsic i args }
    Nothing -> newFree >>= return . M.Var
  -- Unsupported expressions become free varaibles.
  _ -> newFree >>= return . M.Var

intrinsic :: I.ExpOp -> Maybe M.Intrinsic
intrinsic op = case op of
  I.ExpEq   _        -> Just M.Eq
  I.ExpNeq  _        -> Just M.Neq
  I.ExpCond          -> Just M.Cond  
  I.ExpGt   False _  -> Just M.Gt
  I.ExpGt   True  _  -> Just M.Ge
  I.ExpLt   False _  -> Just M.Lt
  I.ExpLt   True  _  -> Just M.Le
  I.ExpNot           -> Just M.Not   
  I.ExpAnd           -> Just M.And   
  I.ExpOr            -> Just M.Or    
  I.ExpMul           -> Just M.Mul   
  I.ExpMod           -> Just M.Mod   
  I.ExpAdd           -> Just M.Add   
  I.ExpSub           -> Just M.Sub   
  I.ExpNegate        -> Just M.Negate
  I.ExpAbs           -> Just M.Abs   
  I.ExpSignum        -> Just M.Signum
  _                  -> Nothing

