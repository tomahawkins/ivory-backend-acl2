-- | Compiling Ivory to ACL2.
module Ivory.Compile.ACL2
  ( compile
  ) where

import qualified Mira      as M
import qualified Mira.CLL  as M
import qualified Mira.Expr as M
import qualified Mira.ACL2 as A

import qualified Ivory.Language.Syntax.AST   as I
import qualified Ivory.Language.Syntax.Type  as I
import qualified Ivory.Language.Syntax.Names as I

-- | Verifies an assertion in a procedure.
verifyAssertion :: [I.Module] -> I.Sym -> I.Stmt -> IO Bool
verifyAssertion modules procName assertion = M.verifyAssertion procs
  where
  procs :: [M.Proc]
  procs = concatMap (cllModule $ Just (procName, assertion)) modules

-- | Verify and remove assertions in an Ivory program.
removeVerifiedAssertions :: [I.Module] -> IO [I.Module]
removeVerifiedAssertions modules = mapM analyzeModule modules
  where
  analyzeModule :: I.Module -> IO I.Module
  analyzeModule m = do
    pub <- mapM analyzeProc $ I.public  $ I.modProcs m
    pri <- mapM analyzeProc $ I.private $ I.modProcs m
    return m { I.modProcs = I.Visible pub pri }

  analyzeProc :: I.Proc -> IO I.Proc
  analyzeProc = return  --XXX

-- | Compiles an Ivory module to ACL2.
compile :: I.Module -> [A.Expr]
compile = M.compile . cllModule Nothing

-- | Convert an Ivory module to CLL.
cllModule :: Maybe (I.Sym, I.Stmt) -> I.Module -> [M.Proc]
cllModule mark = map (cllProc mark) . procs
  where
  procs :: I.Module -> [I.Proc]
  procs m = I.public (I.modProcs m) ++ I.private (I.modProcs m)
  
cllProc :: Maybe (I.Sym, I.Stmt) -> I.Proc -> M.Proc
cllProc mark p = M.Proc (I.procSym p) (map (var . I.tValue) $ I.procArgs p) Nothing requires ensures body
  where
  markStmt :: Maybe I.Stmt
  markStmt = case mark of
    Just (procName, stmt) | I.procSym p == procName -> Just stmt
    _ -> Nothing
  body = map (cllStmt markStmt) (I.procBody p)
  requires :: [M.Expr]
  requires = map (cllExpr . cond . I.getRequire) $ I.procRequires p
  ensures :: [M.Expr -> M.Expr]
  ensures  = map (retval . cllExpr . cond . I.getEnsure ) $ I.procEnsures  p
  cond a = case a of
    I.CondBool a -> a
    I.CondDeref _ _ _ _ -> error $ "CondDeref not supported."
  retval :: M.Expr -> M.Expr -> M.Expr
  retval a ret = retval a
    where
    retval :: M.Expr -> M.Expr
    retval a = case a of
      M.Var "retval" -> ret
      M.Var a -> M.Var a
      M.Literal a -> M.Literal a
      M.Deref a -> M.Deref $ retval a
      M.Array a -> M.Array $ map retval a
      M.Struct a -> M.Struct [ (a, retval b) | (a, b) <- a ]
      M.ArrayIndex a b -> M.ArrayIndex (retval a) (retval b)
      M.StructIndex a b -> M.StructIndex (retval a) b
      M.Intrinsic a b -> M.Intrinsic a $ map retval b

cllStmt :: Maybe I.Stmt -> I.Stmt -> M.Stmt
cllStmt mark a = case a of
  I.IfTE           a b c -> mark' $ M.If (cllExpr a) (cllStmts b) (cllStmts c)
  I.Return         a     -> mark' $ M.Return $ Just $ cllExpr $ I.tValue a
  I.ReturnVoid           -> mark' $ M.Return Nothing
  I.Assert         a     -> mark' $ M.Assert $ cllExpr a
  I.CompilerAssert a     -> mark' $ M.Assert $ cllExpr a
  I.Assume         a     -> mark' $ M.Assume $ cllExpr a
  I.Local          _ a b -> mark' $ M.Let (var a) $ cllInit b
  I.AllocRef       _ a b -> mark' $ M.Block [M.Alloc $ var a, M.Store (M.Var $ var a) (M.Var $ var b)]
  I.Deref          _ a b -> mark' $ M.Let   (var a) $ M.Deref $ cllExpr b
  I.Store          _ a b -> mark' $ M.Store (cllExpr a) (cllExpr b)

  I.Call   _ Nothing  fun args  -> mark' $ M.Call Nothing        (var fun) $ map (cllExpr . I.tValue) args
  I.Call   _ (Just r) fun args  -> mark' $ M.Call (Just $ var r) (var fun) $ map (cllExpr . I.tValue) args
  I.Loop i init incr' body      -> mark' $ M.Loop (var i) (cllExpr init) incr (cllExpr to) (cllStmts body)
    where
    (incr, to) = case incr' of
      I.IncrTo a -> (True, a)
      I.DecrTo a -> (False, a)

  I.Comment _     -> mark' M.Null
  I.RefCopy _ _ _ -> error $ "Unsupported Ivory statement: " ++ show a
  I.Forever _     -> error $ "Unsupported Ivory statement: " ++ show a
  I.Break         -> error $ "Unsupported Ivory statement: " ++ show a
  I.Assign  _ _ _ -> error $ "Unsupported Ivory statement: " ++ show a
  where
  cllStmts :: [I.Stmt] -> [M.Stmt]
  cllStmts = map $ cllStmt mark

  mark' :: M.Stmt -> M.Stmt
  mark' s = case mark of
    Just a' | a' == a -> M.Mark s
    _ -> s


cllInit :: I.Init -> M.Expr
cllInit a = case a of
  I.InitZero      -> M.Literal $ M.LitInteger 0
  I.InitExpr  _ b -> cllExpr b
  I.InitArray a   -> M.Array $ map cllInit a
  I.InitStruct a  -> M.Struct [ (n, cllInit v) | (n, v) <- a ]

cllExpr :: I.Expr -> M.Expr
cllExpr a = case a of
  I.ExpSym a -> M.Var a
  I.ExpVar a -> M.Var $ var a
  I.ExpLit a -> M.Literal $ lit a
  I.ExpOp op args -> M.Intrinsic (cllIntrinsic op) $ map cllExpr args
  I.ExpIndex _ a _ b -> M.ArrayIndex  (M.Deref $ cllExpr a) (cllExpr b)
  I.ExpLabel _ a b   -> M.StructIndex (M.Deref $ cllExpr a) b
  I.ExpToIx a _ -> cllExpr a   -- Is it ok to ignore the maximum bound?
  I.ExpSafeCast _ a -> cllExpr a
  _ -> M.Literal $ M.LitInteger 0
  --error $ "Unsupported Ivory expression: " ++ show a

cllIntrinsic :: I.ExpOp -> M.Intrinsic
cllIntrinsic op = case op of
  I.ExpEq   _        -> M.Eq
  I.ExpNeq  _        -> M.Neq
  I.ExpCond          -> M.Cond  
  I.ExpGt   False _  -> M.Gt
  I.ExpGt   True  _  -> M.Ge
  I.ExpLt   False _  -> M.Lt
  I.ExpLt   True  _  -> M.Le
  I.ExpNot           -> M.Not   
  I.ExpAnd           -> M.And   
  I.ExpOr            -> M.Or    
  I.ExpMul           -> M.Mul   
  I.ExpMod           -> M.Mod   
  I.ExpAdd           -> M.Add   
  I.ExpSub           -> M.Sub   
  I.ExpNegate        -> M.Negate
  I.ExpAbs           -> M.Abs   
  I.ExpSignum        -> M.Signum
  a                -> error $ "Unsupported Ivory intrinsic: " ++ show a

lit :: I.Literal -> M.Literal
lit a = case a of
  I.LitInteger a -> M.LitInteger a
  I.LitFloat   a -> M.LitFloat   a
  I.LitDouble  a -> M.LitDouble  a
  I.LitChar    a -> M.LitChar    a
  I.LitBool    a -> M.LitBool    a
  I.LitNull      -> M.LitNull     
  I.LitString  a -> M.LitString  a


class GetVar a where var :: a -> M.Var

instance GetVar I.Var where
  var a = case a of
    I.VarName     a -> a
    I.VarInternal a -> a
    I.VarLitName  a -> a

instance GetVar I.Name where
  var a = case a of
    I.NameSym a -> a
    I.NameVar a -> var a
