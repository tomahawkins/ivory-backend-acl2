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

import Mira.ACL2 hiding (var, lit)
import Mira.ACL2ConvertCPS
import Mira.ACL2ConvertRTL
import qualified Mira.CLL as C
import Mira.CLL (Var, Literal (..))
import Mira.CPS (explicitStack)
import Mira.CPSConvert
import Mira.RTLConvert

import qualified Ivory.Language.Syntax.AST as I
import Ivory.Language.Syntax.AST (Module (..), ExpOp (..))
import Ivory.Language.Syntax.Type
import qualified Ivory.Language.Syntax.Names as I

-- | Compiles a module to two different ACL2 representations: assembly and CPS.
compileModule :: Module -> (String, [Expr], [Expr])
compileModule m = (name, acl21, acl22)
  where
  cll   = map cllConvert $ procs m
  cps1  = cpsConvert cll
  cps2  = map explicitStack cps1
  rtl   = rtlConvert        cps2
  acl21 = acl2ConvertRTL intrinsicCode intrinsicImp rtl 
  acl22 = acl2ConvertCPS intrinsics cps2
  name = modName m
  procs :: I.Module -> [I.Proc]
  procs m = I.public (I.modProcs m) ++ I.private (I.modProcs m)

-- Intrinsic implementation for ACL2 (CPS form).
intrinsics :: ExpOp -> [Expr] -> Expr
intrinsics op args = case op of
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
  ExpAdd           -> arg 0 + arg 1
  ExpSub           -> arg 0 - arg 1
  ExpNegate        -> 0 - arg 1
  ExpAbs           -> if' (call ">=" [arg 0, 0]) (arg 0) (0 - arg 0)
  ExpSignum        -> if' (call ">"  [arg 0, 0]) 1 $ if' (call "<" [arg 0, 0]) (-1) 0
  a -> error $ "Unsupported intrinsic: " ++ show a
  where
  arg n = args !! n

intrinsicCode :: ExpOp -> Expr
intrinsicCode a = case a of
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

intrinsicImp :: Expr -> (Int -> Expr) -> Expr
intrinsicImp op arg = case' op
  [ (codeExpEq            , if' (equal (arg 0) (arg 1)) 1 0)
  , (codeExpNeq           , if' (not' (equal (arg 0) (arg 1))) 1 0)
  , (codeExpCond          , if' (zip' (arg 0)) (arg 2) (arg 1))
  , (codeExpGt            , if' (call ">"  [arg 0, arg 1]) 1 0)
  , (codeExpGe            , if' (call ">=" [arg 0, arg 1]) 1 0)
  , (codeExpLt            , if' (call "<"  [arg 0, arg 1]) 1 0)
  , (codeExpLe            , if' (call "<=" [arg 0, arg 1]) 1 0)
  , (codeExpNot           , if' (zip' (arg 0)) 1 0)
  , (codeExpAnd           , if' (or'  (zip' (arg 0)) (zip' (arg 1))) 0 1)
  , (codeExpOr            , if' (and' (zip' (arg 0)) (zip' (arg 1))) 0 1)
  , (codeExpMul           , arg 0 * arg 1)
  , (codeExpAdd           , arg 0 + arg 1)
  , (codeExpSub           , arg 0 - arg 1)
  , (codeExpNegate        , 0 - arg 1)
  , (codeExpAbs           , if' (call ">=" [arg 0, 0]) (arg 0) (0 - arg 0))
  , (codeExpSignum        , if' (call ">"  [arg 0, 0]) 1 $ if' (call "<" [arg 0, 0]) (-1) 0)
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
  (_, result, _) <- readProcessWithExitCode "acl2" [] acl2Asm
  let pass = expected == (not $ any (isPrefixOf "ACL2 Error") $ lines result)
  putStrLn $ if pass then "pass" else "FAIL"
  writeFile (name ++ "_assertions.log") result
  hFlush stdout

  return $ terminates && pass

  where
  (name, acl2Asm', _) = compileModule m
  acl2Asm = unlines $ map show acl2Asm'

-- | Verifies a list of modules.
verifyModules :: [(Bool, Module)] -> IO Bool
verifyModules m = do
  pass <- sequence [ verifyModule a b | (a, b) <- m ]
  return $ and pass

-- | Verifies termination of a module.
verifyTermination :: Module -> IO Bool
verifyTermination m = do
  writeFile (name ++ ".lisp") acl2CPS
  (_, result, _) <- readProcessWithExitCode "acl2" [] acl2CPS
  let terminates = not $ any (isPrefixOf "ACL2 Error") $ lines result
  writeFile (name ++ "_termination.log") result
  return terminates
  where
  (name, _, acl2CPS') = compileModule m
  acl2CPS = unlines $ map show acl2CPS'

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
  I.Assign   _ a b              -> C.Let (var a) $ cllExpr b
  I.AllocRef _ a b              -> C.Let (var a) $ C.Var $ var b
  I.Local  _ a (I.InitExpr _ b) -> C.Let (var a) $ cllExpr b
  I.Call   _ Nothing  fun args  -> C.Call Nothing        (var fun) $ map (cllExpr . tValue) args
  I.Call   _ (Just r) fun args  -> C.Call (Just $ var r) (var fun) $ map (cllExpr . tValue) args
  I.Loop i init incr' body      -> C.Loop (var i) (cllExpr init) incr (cllExpr to) (cllStmts body)
    where
    (incr, to) = case incr' of
      I.IncrTo a -> (True, a)
      I.DecrTo a -> (False, a)
  a -> error $ "Unsupported Ivory statement: " ++ show a

{-
cpsStmts :: [I.Stmt] -> Cont I.ExpOp -> CPS (Cont I.ExpOp)
cpsStmts a cont = case a of
  [] -> return cont
  a : b -> do
    cont <- cpsStmts b cont
    case a of
      I.IfTE a b c -> do
        f <- genVar
        let args = contFreeVars cont
        addProc f args cont
        b <- cpsStmts b $ Call f args Nothing
        c <- cpsStmts c $ Call f args Nothing
        cpsExpr a $ \ a -> return $ If a b c
      I.Return a   -> cpsExpr (tValue a) $ \ a -> return $ Return $ Just a  -- This ignores cont (the rest of the function).  Is this ok?
      I.ReturnVoid -> return $ Return Nothing  -- Again, ignores cont.
      I.Assert         a -> cpsExpr a $ \ a -> return $ Assert a cont
      I.CompilerAssert a -> cpsExpr a $ \ a -> return $ Assert a cont
      I.Assume         a -> cpsExpr a $ \ a -> return $ Assume a cont
      I.Assign _ a b -> cpsExpr b $ \ b -> return $ Let (var a) (Var b) cont
      I.Local  _ a (I.InitExpr _ b) -> cpsExpr b $ \ b -> return $ Let (var a) (Var b) cont  
      I.Call _ Nothing fun args -> f [] $ map tValue args
        where
        f :: [Var] -> [I.Expr] -> CPS (Cont I.ExpOp)
        f args a = case a of
          [] -> return $ Call (var fun) args $ Just cont
          a : b -> cpsExpr a $ \ a -> f (args ++ [a]) b
      I.Call _ (Just result) fun args -> f [] $ map tValue args
        where
        f :: [Var] -> [I.Expr] -> CPS (Cont I.ExpOp)
        f args a = case a of
          [] -> return $ Call (var fun) args $ Just $ Let (var result) (Var "retval") cont
          a : b -> cpsExpr a $ \ a -> f (args ++ [a]) b
      I.AllocRef _ a b -> return $ Let (var a) (Var $ var b) cont
      I.Loop i' init incr' body -> do  -- XXX Need to add a check to ensure loop body doesn't have any return or break statements.
        body <- cpsStmts body Halt
        let i = var i'
            args = delete i $ contFreeVars body
        cpsExpr init $ \ init -> cpsExpr to $ \ to -> do
          f <- genVar
          addProc f (i : args) $ body --XXX Need to add conditional and replace Halt with recursive call and return.
          return $ Call f (init : args) $ Just cont
        where
        (incr, to) = case incr' of
          I.IncrTo a -> (True, a)
          I.DecrTo a -> (False, a)

      a -> error $ "Unsupported statement: " ++ show a

-}

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
