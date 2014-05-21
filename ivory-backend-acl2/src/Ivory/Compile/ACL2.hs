-- | Compiling Ivory to ACL2.
module Ivory.Compile.ACL2
  ( compileModule
  , verifyModule
  , verifyModules
  , verifyTermination
  ) where

import Data.List
import System.IO
import System.Process

import Mira
import qualified Mira.CLL as C
import Mira.Expr

import qualified Ivory.Language.Syntax.AST as I
import Ivory.Language.Syntax.AST (Module (..), ExpOp (..))
import Ivory.Language.Syntax.Type
import qualified Ivory.Language.Syntax.Names as I

-- | Compiles a module to two different ACL2 representations: assembly and CPS.
compileModule :: Module -> IO String
compileModule m = do
  compile name $ map cllConvert $ procs m
  return name
  where
  name = modName m
  procs :: I.Module -> [I.Proc]
  procs m = I.public (I.modProcs m) ++ I.private (I.modProcs m)

-- | Given a expected result, verifies a module.
verifyModule :: Bool -> Module -> IO Bool
verifyModule expected m = do
  putStr $ "Verifying termination of: " ++ name ++ " ... "
  hFlush stdout
  terminates <- verifyTermination m
  putStrLn $ if terminates then "pass" else "FAIL"
  hFlush stdout

  putStr $ "Verifying assertions of:  " ++ name ++ " ... "
  hFlush stdout
  f <- readFile $ modName m ++ "-rtl.lisp"
  (_, result, _) <- readProcessWithExitCode "acl2" [] f
  let pass = expected == (not $ any (isPrefixOf "ACL2 Error") $ lines result)
  putStrLn $ if pass then "pass" else "FAIL"
  writeFile (name ++ "_assertions.log") result
  hFlush stdout

  return $ terminates && pass
  where
  name = modName m

-- | Verifies a list of modules.
verifyModules :: [(Bool, Module)] -> IO Bool
verifyModules m = do
  pass <- sequence [ verifyModule a b | (a, b) <- m ]
  return $ and pass

-- | Verifies termination of a module.
verifyTermination :: Module -> IO Bool
verifyTermination m = do
  name <- compileModule m
  f <- readFile $ name ++ "-cps.lisp"
  (_, result, _) <- readProcessWithExitCode "acl2" [] f
  let terminates = not $ any (isPrefixOf "ACL2 Error") $ lines result
  writeFile (name ++ "_termination.log") result
  return terminates

cllConvert :: I.Proc -> C.Proc
cllConvert p = C.Proc (I.procSym p) (map (var . tValue) $ I.procArgs p) Nothing $ map cllStmt body
  where
  body = requires ++ insertEnsures (I.procBody p)
  requires = map (I.Assert . cond . I.getRequire) $ I.procRequires p
  ensures :: I.Expr -> I.Stmt
  --XXX Need to replace all the retvals in ensures conditions with the return expression.
  --ensures  = map (I.Assert . cond . I.getEnsure ) $ I.procEnsures  p
  ensures = const $ I.Assert $ I.ExpLit $ I.LitBool True
  cond a = case a of
    I.CondBool a -> a
    I.CondDeref _ _ _ _ -> error $ "CondDeref not supported."

  insertEnsures' :: I.Stmt -> [I.Stmt]
  insertEnsures' a = case a of
    I.IfTE a b c -> [I.IfTE a (insertEnsures b) (insertEnsures c)]
    I.Loop a b c d -> [I.Loop a b c $ insertEnsures d]
    I.Forever a -> [I.Forever $ insertEnsures a]
    I.Return a   -> [ensures $ tValue a, I.Return a]
    I.ReturnVoid -> [I.ReturnVoid]
    a -> [a]

  insertEnsures :: [I.Stmt] -> [I.Stmt]
  insertEnsures = concatMap insertEnsures'

cllStmts :: [I.Stmt] -> [C.Stmt]
cllStmts = map cllStmt

cllStmt :: I.Stmt -> C.Stmt
cllStmt a = case a of
  I.IfTE           a b c -> C.If (cllExpr a) (cllStmts b) (cllStmts c)
  I.Return         a     -> C.Return $ Just $ cllExpr $ tValue a
  I.ReturnVoid           -> C.Return Nothing
  I.Assert         a     -> C.Assert $ cllExpr a
  I.CompilerAssert a     -> C.Assert $ cllExpr a
  I.Assume         a     -> C.Assume $ cllExpr a
  I.Local          _ a b -> C.Let (var a) $ cllInit b
  I.AllocRef       _ a b -> C.Block [C.Let (var a) Alloc, C.Store (C.Var $ var a) (C.Var $ var b)]
  I.Deref          _ a b -> C.Let   (var a) $ Deref $ cllExpr b
  I.Store          _ a b -> C.Store (cllExpr a) (cllExpr b)

  I.Call   _ Nothing  fun args  -> C.Call Nothing        (var fun) $ map (cllExpr . tValue) args
  I.Call   _ (Just r) fun args  -> C.Call (Just $ var r) (var fun) $ map (cllExpr . tValue) args
  I.Loop i init incr' body      -> C.Loop (var i) (cllExpr init) incr (cllExpr to) (cllStmts body)
    where
    (incr, to) = case incr' of
      I.IncrTo a -> (True, a)
      I.DecrTo a -> (False, a)

  I.RefCopy _ _ _ -> error $ "Unsupported Ivory statement: " ++ show a
  I.Forever _     -> error $ "Unsupported Ivory statement: " ++ show a
  I.Break         -> error $ "Unsupported Ivory statement: " ++ show a
  I.Comment _     -> error $ "Unsupported Ivory statement: " ++ show a
  I.Assign  _ _ _ -> error $ "Unsupported Ivory statement: " ++ show a

cllInit :: I.Init -> C.Expr
cllInit a = case a of
  I.InitZero      -> Literal $ LitInteger 0
  I.InitExpr  _ b -> cllExpr b
  I.InitArray a   -> Array $ map cllInit a
  I.InitStruct a  -> Struct [ (n, cllInit v) | (n, v) <- a ]

cllExpr :: I.Expr -> C.Expr
cllExpr a = case a of
  I.ExpSym a -> Var a
  I.ExpVar a -> Var $ var a
  I.ExpLit a -> Literal $ lit a
  I.ExpOp op args -> Intrinsic (cllIntrinsic op) $ map cllExpr args
  I.ExpIndex _ a _ b -> ArrayIndex (cllExpr a) (cllExpr b)
  I.ExpLabel _ a b   -> StructIndex (cllExpr a) b
  I.ExpToIx a _ -> cllExpr a   -- Is it ok to ignore the maximum bound?
  I.ExpSafeCast _ a -> cllExpr a
  a -> error $ "Unsupported Ivory expression: " ++ show a

cllIntrinsic :: ExpOp -> Intrinsic
cllIntrinsic op = case op of
  ExpEq   _        -> Eq
  ExpNeq  _        -> Neq
  ExpCond          -> Cond  
  ExpGt   False _  -> Gt
  ExpGt   True  _  -> Ge
  ExpLt   False _  -> Lt
  ExpLt   True  _  -> Le
  ExpNot           -> Not   
  ExpAnd           -> And   
  ExpOr            -> Or    
  ExpMul           -> Mul   
  ExpMod           -> Mod   
  ExpAdd           -> Add   
  ExpSub           -> Sub   
  ExpNegate        -> Negate
  ExpAbs           -> Abs   
  ExpSignum        -> Signum
  a                -> error $ "Unsupported Ivory intrinsic: " ++ show a

lit :: I.Literal -> Literal
lit a = case a of
  I.LitInteger a -> LitInteger a
  I.LitFloat   a -> LitFloat   a
  I.LitDouble  a -> LitDouble  a
  I.LitChar    a -> LitChar    a
  I.LitBool    a -> LitBool    a
  I.LitNull      -> LitNull     
  I.LitString  a -> LitString  a


class GetVar a where var :: a -> Var

instance GetVar I.Var where
  var a = case a of
    I.VarName     a -> a
    I.VarInternal a -> a
    I.VarLitName  a -> a

instance GetVar I.Name where
  var a = case a of
    I.NameSym a -> a
    I.NameVar a -> var a
