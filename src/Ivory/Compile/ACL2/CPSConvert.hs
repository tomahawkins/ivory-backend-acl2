-- | Convert Ivory procedures to CPS.
module Ivory.Compile.ACL2.CPSConvert
  ( cpsConvertProc
  , varSym
  ) where

import MonadLib

import Ivory.Compile.ACL2.CPS
import qualified Ivory.Language.Syntax.AST   as I
import qualified Ivory.Language.Syntax.Names as I
import Ivory.Language.Syntax.Type


cpsConvertProc :: I.Proc -> Proc I.ExpOp
cpsConvertProc p = Proc (I.procSym p) (map (varSym . tValue) $ I.procArgs p) cont
  where
  (cont, _) = runId $ runStateT 0 $ cpsStmts (requires ++ insertEnsures (I.procBody p)) Halt
  requires = map (I.Assert . cond . I.getRequire) $ I.procRequires p
  ensures  = map (I.Assert . cond . I.getEnsure ) $ I.procEnsures  p
  cond a = case a of
    I.CondBool a -> a
    I.CondDeref _ _ _ _ -> error $ "CondDeref not supported."

  insertEnsures' :: I.Stmt -> [I.Stmt]
  insertEnsures' a = case a of
    I.IfTE a b c -> [I.IfTE a (insertEnsures b) (insertEnsures c)]
    I.Loop a b c d -> [I.Loop a b c $ insertEnsures d]
    I.Forever a -> [I.Forever $ insertEnsures a]
    I.Return a -> ensures ++ [I.Return a]
    I.ReturnVoid -> ensures ++ [I.ReturnVoid]
    a -> [a]

  insertEnsures :: [I.Stmt] -> [I.Stmt]
  insertEnsures = concatMap insertEnsures'

type CPS = StateT Int Id

gensym :: CPS Var
gensym = do
  i <- get
  set $ i + 1
  return $ "_cpsConvert" ++ show i

cpsStmts :: [I.Stmt] -> Cont I.ExpOp -> CPS (Cont I.ExpOp)
cpsStmts a cont = case a of
  [] -> return $ Halt
  a : b -> do
    cont <- cpsStmts b cont
    case a of
      I.IfTE a b c -> do
        b <- cpsStmts b cont
        c <- cpsStmts c cont
        cpsExpr a $ \ a -> return $ If a b c
      I.Return a   -> cpsExpr (tValue a) $ \ a -> return $ Return $ Just a  -- This ignores cont (the rest of the function).  Is this ok?
      I.ReturnVoid -> return $ Return Nothing  -- Again, ignores cont.
      I.Assert         a -> cpsExpr a $ \ a -> return $ Assert a cont
      I.CompilerAssert a -> cpsExpr a $ \ a -> return $ Assert a cont
      I.Assume         a -> cpsExpr a $ \ a -> return $ Assume a cont
      I.Assign _ a b -> cpsExpr b $ \ b -> return $ Let (varSym a) (Var b) cont
      I.Local  _ a (I.InitExpr _ b) -> cpsExpr b $ \ b -> return $ Let (varSym a) (Var b) cont  
      I.Call _ Nothing fun args -> f [] $ map tValue args
        where
        f :: [Var] -> [I.Expr] -> CPS (Cont I.ExpOp)
        f args a = case a of
          [] -> return $ Call (nameSym fun) args cont
          a : b -> cpsExpr a $ \ a -> f (args ++ [a]) b
      I.Call _ (Just result) fun args -> f [] $ map tValue args
        where
        f :: [Var] -> [I.Expr] -> CPS (Cont I.ExpOp)
        f args a = case a of
          [] -> return $ Call (nameSym fun) args $ Let (varSym result) (Var "retval") cont
          a : b -> cpsExpr a $ \ a -> f (args ++ [a]) b
      a -> error $ "Unsupported statement: " ++ show a

cpsExpr :: I.Expr -> (Var -> CPS (Cont I.ExpOp)) -> CPS (Cont I.ExpOp)
cpsExpr a k = case a of
  I.ExpSym a -> k a
  I.ExpVar a -> k $ varSym a
  I.ExpLit a -> do
    v <- gensym
    cont <- k v
    return $ Let v (Literal $ lit a) cont
  I.ExpOp op args -> f args []
    where
    f :: [I.Expr] -> [Var] -> CPS (Cont I.ExpOp)
    f argsE argsV = case argsE of
      [] -> do
        v <- gensym
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

varSym :: I.Var -> Var
varSym a = case a of
  I.VarName     a -> a
  I.VarInternal a -> a
  I.VarLitName  a -> a

nameSym :: I.Name -> Var
nameSym a = case a of
  I.NameSym a -> a
  I.NameVar a -> varSym a

