-- | Convert Ivory procedures to CPS.
module Ivory.Compile.ACL2.CPSConvert
  ( cpsConvert
  ) where

import Data.List (delete)
import MonadLib

import Mira.CPS

import qualified Ivory.Language.Syntax.AST   as I
import qualified Ivory.Language.Syntax.Names as I
import Ivory.Language.Syntax.Type

cpsConvert :: [I.Proc] -> [Proc I.ExpOp]
cpsConvert = snd . snd . runId . runStateT (0, []) . mapM cpsConvertProc

type CPS = StateT (Int, [Proc I.ExpOp]) Id

addProc :: Var -> [Var] -> Cont I.ExpOp -> CPS ()
addProc f args cont = do
  (i, procs) <- get
  set (i, procs ++ [Proc f args cont])

cpsConvertProc :: I.Proc -> CPS ()
cpsConvertProc p = do
  cont <- cpsStmts (requires ++ insertEnsures (I.procBody p)) Halt
  addProc (I.procSym p) (map (var . tValue) $ I.procArgs p) cont
  where
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

genVar :: CPS Var
genVar = do
  (i, p) <- get
  set (i + 1, p)
  return $ "_cps" ++ show i

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

cpsExpr :: I.Expr -> (Var -> CPS (Cont I.ExpOp)) -> CPS (Cont I.ExpOp)
cpsExpr a k = case a of
  I.ExpSym a -> k a
  I.ExpVar a -> k $ var a
  I.ExpLit a -> do
    v <- genVar
    cont <- k v
    return $ Let v (Literal $ lit a) cont
  I.ExpOp op args -> f args []
    where
    f :: [I.Expr] -> [Var] -> CPS (Cont I.ExpOp)
    f argsE argsV = case argsE of
      [] -> do
        v <- genVar
        cont <- k v
        return $ Let v (Intrinsic op argsV) cont
      a : b -> cpsExpr a $ \ a -> f b (argsV ++ [a])
  _ -> error "Unsupported expression."

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

