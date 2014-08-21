-- | Verifies and removes assertions from an Ivory program.
module Ivory.Opts.Asserts
  ( assertsFold
  ) where

import MonadLib

import qualified Ivory.Language.Syntax.AST as I
import qualified Mira.ACL2 as A

-- Data carried through verification monad.
data VDB = VDB
  { nextVCId
  , nextBCId
  , nextStateId
  , nextEnvId   :: Int
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
      b <- runStateT undefined $ block $ I.procBody proc
      return $ proc { I.procBody = fst b }

block :: I.Block -> V I.Block
block = mapM stmt

-- Defines a new verification condition (VC).
newVC :: I.Expr -> V A.Expr
newVC check = do
  m <- get
  let vc = "_vc" ++ show (nextVCId m)
  set m { nextVCId = nextVCId m + 1, body = body m . A.let' [(vc, A.implies (branch m) $ bool check)] }
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
  m <- get
  let bc = "_bc" ++ show (nextBCId m)
  set m { nextBCId = nextBCId m + 1, body = body m . A.let' [(bc, bool cond)] }
  return $ A.var bc

stmt :: I.Stmt -> V I.Stmt
stmt a = case a of
  I.Assert         b -> checkAssert a b
  I.CompilerAssert b -> checkAssert a b
  I.Assume         b -> newVC b >>= addVC >> return a

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

  {-
  I.Deref  _ var ref   ->
  I.Store  _ ref value -> 
  I.Assign _ var value ->
  -}
  _ -> undefined

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


expr :: I.Expr -> A.Expr
expr = undefined

bool :: I.Expr -> A.Expr
bool = A.not' . A.zip' . expr

