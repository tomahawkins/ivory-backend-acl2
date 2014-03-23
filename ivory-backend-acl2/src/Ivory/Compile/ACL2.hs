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
import Mira.ACL2 hiding (var, lit)
import qualified Mira.CLL as C
import Mira.CLL (Var, Literal (..))
import Mira.Intrinsics

import qualified Ivory.Language.Syntax.AST as I
import Ivory.Language.Syntax.AST (Module (..), ExpOp (..))
import Ivory.Language.Syntax.Type
import qualified Ivory.Language.Syntax.Names as I

-- | Compiles a module to two different ACL2 representations: assembly and CPS.
compileModule :: Module -> IO String
compileModule m = do
  compile name intrinsicImp $ map cllConvert $ procs m
  return name
  where
  name = modName m
  procs :: I.Module -> [I.Proc]
  procs m = I.public (I.modProcs m) ++ I.private (I.modProcs m)

instance Intrinsics ExpOp where
  intrinsicAdd = ExpAdd
  intrinsicSub = ExpSub
  intrinsicGE  = ExpGt True TyVoid
  intrinsicLE  = ExpLt True TyVoid

  intrinsicImpl op arg = case op of
    ExpEq   _        -> if' (equal (arg 0) (arg 1)) 1 0
    ExpNeq  _        -> if' (not' (equal (arg 0) (arg 1))) 1 0
    ExpCond          -> if' (zip' (arg 0)) (arg 2) (arg 1)
    ExpGt   False _  -> if' (call ">"  [arg 0, arg 1]) 1 0
    ExpGt   True  _  -> if' (call ">=" [arg 0, arg 1]) 1 0
    ExpLt   False _  -> if' (call "<"  [arg 0, arg 1]) 1 0
    ExpLt   True  _  -> if' (call "<=" [arg 0, arg 1]) 1 0
    ExpNot           -> if' (zip' (arg 0)) 1 0
    ExpAnd           -> if' (or'  (zip' (arg 0)) (zip' (arg 1))) 0 1
    ExpOr            -> if' (and' (zip' (arg 0)) (zip' (arg 1))) 0 1
    ExpMul           -> arg 0 * arg 1
    ExpMod           -> mod' (arg 0) (arg 1) 
    ExpAdd           -> arg 0 + arg 1
    ExpSub           -> arg 0 - arg 1
    ExpNegate        -> 0 - arg 1
    ExpAbs           -> if' (call ">=" [arg 0, 0]) (arg 0) (0 - arg 0)
    ExpSignum        -> if' (call ">"  [arg 0, 0]) 1 $ if' (call "<" [arg 0, 0]) (-1) 0
    a -> error $ "Unsupported intrinsic: " ++ show a

  intrinsicEncode a = case a of
    ExpEq  _         -> codeExpEq           
    ExpNeq _         -> codeExpNeq          
    ExpCond          -> codeExpCond         
    ExpGt False _    -> codeExpGt           
    ExpGt True  _    -> codeExpGe           
    ExpLt False _    -> codeExpLt           
    ExpLt True  _    -> codeExpLe           
    ExpNot           -> codeExpNot          
    ExpAnd           -> codeExpAnd          
    ExpOr            -> codeExpOr           
    ExpMul           -> codeExpMul          
    ExpMod           -> codeExpMod
    ExpAdd           -> codeExpAdd          
    ExpSub           -> codeExpSub          
    ExpNegate        -> codeExpNegate       
    ExpAbs           -> codeExpAbs          
    ExpSignum        -> codeExpSignum       
    a -> error $ "ExpOp not supported: " ++ show a

codeExpEq            = 0
codeExpNeq           = 1
codeExpCond          = 2
codeExpGt            = 3
codeExpGe            = 4
codeExpLt            = 5
codeExpLe            = 6
codeExpNot           = 7
codeExpAnd           = 8
codeExpOr            = 9
codeExpMul           = 10
codeExpAdd           = 11
codeExpSub           = 12
codeExpNegate        = 13
codeExpAbs           = 14
codeExpSignum        = 15
codeExpMod           = 16

intrinsicImp :: Expr -> (Int -> Expr) -> Expr
intrinsicImp op arg = case' op
  [ (fromIntegral codeExpEq     , if' (equal (arg 0) (arg 1)) 1 0)
  , (fromIntegral codeExpNeq    , if' (not' (equal (arg 0) (arg 1))) 1 0)
  , (fromIntegral codeExpCond   , if' (zip' (arg 0)) (arg 2) (arg 1))
  , (fromIntegral codeExpGt     , if' (call ">"  [arg 0, arg 1]) 1 0)
  , (fromIntegral codeExpGe     , if' (call ">=" [arg 0, arg 1]) 1 0)
  , (fromIntegral codeExpLt     , if' (call "<"  [arg 0, arg 1]) 1 0)
  , (fromIntegral codeExpLe     , if' (call "<=" [arg 0, arg 1]) 1 0)
  , (fromIntegral codeExpNot    , if' (zip' (arg 0)) 1 0)
  , (fromIntegral codeExpAnd    , if' (or'  (zip' (arg 0)) (zip' (arg 1))) 0 1)
  , (fromIntegral codeExpOr     , if' (and' (zip' (arg 0)) (zip' (arg 1))) 0 1)
  , (fromIntegral codeExpMul    , arg 0 * arg 1)
  , (fromIntegral codeExpMod    , mod' (arg 0) (arg 1))
  , (fromIntegral codeExpAdd    , arg 0 + arg 1)
  , (fromIntegral codeExpSub    , arg 0 - arg 1)
  , (fromIntegral codeExpNegate , 0 - arg 1)
  , (fromIntegral codeExpAbs    , if' (call ">=" [arg 0, 0]) (arg 0) (0 - arg 0))
  , (fromIntegral codeExpSignum , if' (call ">"  [arg 0, 0]) 1 $ if' (call "<" [arg 0, 0]) (-1) 0)
  ] 0

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

cllConvert :: I.Proc -> C.Proc ExpOp
cllConvert p = C.Proc (I.procSym p) (map (var . tValue) $ I.procArgs p) $ map cllStmt body
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

cllStmts :: [I.Stmt] -> [C.Stmt ExpOp]
cllStmts = map cllStmt

cllStmt :: I.Stmt -> C.Stmt ExpOp
cllStmt a = case a of
  I.IfTE a b c -> C.If (cllExpr a) (cllStmts b) (cllStmts c)
  I.Return a   -> C.Return $ Just $ cllExpr $ tValue a
  I.ReturnVoid -> C.Return Nothing
  I.Assert         a -> C.Assert $ cllExpr a
  I.CompilerAssert a -> C.Assert $ cllExpr a
  I.Assume         a -> C.Assume $ cllExpr a
  I.Local    _ a (I.InitExpr _ b) -> C.Let (var a) $ cllExpr b

  -- What are the right semantics for the following?
  I.Assign   _ a b -> C.Let (var a) $ cllExpr b
  I.AllocRef _ a b -> C.Let (var a) $ C.Var $ var b
  I.Deref    _ a b -> C.Let (var a) $ cllExpr b
  I.Store    _ (I.ExpVar a) b -> C.Let (var a) $ cllExpr b

  I.Call   _ Nothing  fun args  -> C.Call Nothing        (var fun) $ map (cllExpr . tValue) args
  I.Call   _ (Just r) fun args  -> C.Call (Just $ var r) (var fun) $ map (cllExpr . tValue) args
  I.Loop i init incr' body      -> C.Loop (var i) (cllExpr init) incr (cllExpr to) (cllStmts body)
    where
    (incr, to) = case incr' of
      I.IncrTo a -> (True, a)
      I.DecrTo a -> (False, a)
  a -> error $ "Unsupported Ivory statement: " ++ show a

cllExpr :: I.Expr -> C.Expr ExpOp
cllExpr a = case a of
  I.ExpSym a -> C.Var a
  I.ExpVar a -> C.Var $ var a
  I.ExpLit a -> C.Lit $ lit a
  I.ExpOp op args -> C.Intrinsic op $ map cllExpr args
  a -> error $ "Unsupported Ivory expression: " ++ show a

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
