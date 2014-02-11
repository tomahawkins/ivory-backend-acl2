-- | Convert Ivory procedures to CPS.
module Ivory.Compile.ACL2.CPSConvert
  ( cpsConvertProc
  , varSym
  ) where

import MonadLib

import Ivory.Compile.ACL2.CPS
import qualified Ivory.Language.Syntax.AST as I
import Ivory.Language.Syntax.Names
import Ivory.Language.Syntax.Type


cpsConvertProc :: I.Proc -> Proc
cpsConvertProc p = Proc (I.procSym p) (map (varSym . tValue) $ I.procArgs p) cont
  where
  (cont, _) = runId $ runStateT (0, 0) $ cpsStmts (I.procBody p) Halt

type CPS = StateT (Int, Int) Id

gensym :: CPS Sym
gensym = do
  (i, l) <- get
  set (i + 1, l)
  return $ "_" ++ show i

withLoop :: CPS a -> CPS a
withLoop a = do
  (i, l) <- get
  set (i, l + 1)
  a <- a
  (i, l) <- get
  set (i, l - 1)
  return a

popLoops :: Cont -> CPS Cont
popLoops a = do
  (_, l) <- get
  return $ f l
  where
  f :: Int -> Cont
  f i
    | i <= 0    = a
    | otherwise = Pop $ f $ i - 1

cpsStmts :: [I.Stmt] -> Cont -> CPS Cont
cpsStmts a cont = case a of
  [] -> return $ Halt
  a : b -> do
    cont <- cpsStmts b cont
    case a of
      I.IfTE a b c -> do
        b <- cpsStmts b cont
        c <- cpsStmts c cont
        cpsExpr a $ \ a -> return $ If a b c
      I.Return a   -> cpsExpr (tValue a) $ \ a -> popLoops $ Return $ Just a  -- This ignores cont (the rest of the function).  Is this ok?
      I.ReturnVoid -> popLoops $ Return Nothing  -- Again, ignores cont.
      I.Assert         a -> cpsExpr a $ \ a -> return $ Assert a cont
      I.CompilerAssert a -> cpsExpr a $ \ a -> return $ Assert a cont
      I.Assume         a -> cpsExpr a $ \ a -> return $ Assume a cont
      I.Deref  _ a b -> cpsExpr b $ \ b -> return $ Let (varSym a) (Deref b) cont
      I.Store  _ a b -> cpsExpr a $ \ a -> cpsExpr b $ \ b -> return $ Store a b cont  -- Assumes a is evaluated before b in a = b.
      I.Assign _ a b -> cpsExpr b $ \ b -> return $ Assign (varSym a) b cont
      I.Local  _ a (I.InitExpr _ b) -> cpsExpr b $ \ b -> return $ Let (varSym a) (SValue b) cont  
      I.Call _ Nothing fun args -> f [] $ map tValue args
        where
        f :: [SValue] -> [I.Expr] -> CPS Cont
        f args a = case a of
          [] -> return $ Push cont $ Call (nameSym fun) args 
          a : b -> cpsExpr a $ \ a -> f (args ++ [a]) b
      I.Call _ (Just result) fun args -> f [] $ map tValue args
        where
        f :: [SValue] -> [I.Expr] -> CPS Cont
        f args a = case a of
          [] -> return $ Push (Let (varSym result) (SValue ReturnValue) cont) $ Call (nameSym fun) args 
          a : b -> cpsExpr a $ \ a -> f (args ++ [a]) b
      I.Forever a -> do
        loop <- withLoop $ cpsStmts a Halt
        return $ Push cont $ Forever loop
      I.Break -> do
        return $ Return Nothing
      -- XXX Should rewrite loops into forevers with breaks.
      I.Loop v i incr block -> cpsExpr i $ \ i -> cpsExpr to $ \ to -> do
        loop <- withLoop $ cpsStmts block Halt
        return $ Push cont $ Loop (varSym v) i dir to loop cont
        where
        (dir, to) = case incr of
          I.IncrTo to -> (Incr, to)
          I.DecrTo to -> (Decr, to)
      I.RefCopy  _ _ _ -> error "RefCopy not supported."
      I.AllocRef _ _ _ -> error "AllocRef not supported."
      I.Local _ _ I.InitZero -> error "Local _ _ InitZero not supported."
      I.Local _ _ (I.InitStruct _) -> error "Local _ _ (InitStruct _) not supported."
      I.Local _ _ (I.InitArray _) -> error "Local _ _ (InitArray _) not supported."

cpsExpr :: I.Expr -> (SValue -> CPS Cont) -> CPS Cont
cpsExpr a k = case a of
  I.ExpSym a -> k $ Sym a
  I.ExpVar a -> k $ Sym $ varSym a
  I.ExpLit a -> k $ Literal a
  I.ExpOp op args -> f args []
    where
    f :: [I.Expr] -> [SValue] -> CPS Cont
    f argsE argsV = case argsE of
      [] -> do
        v <- gensym
        cont <- k $ Sym v
        return $ Let v (Intrinsic (IntrinsicOp op) argsV) cont
      a : b -> cpsExpr a $ \ a -> f b (argsV ++ [a])
  _ -> error "Unsupported expression."

varSym :: Var -> Sym
varSym a = case a of
  VarName     a -> a
  VarInternal a -> a
  VarLitName  a -> a

nameSym :: I.Name -> Sym
nameSym a = case a of
  I.NameSym a -> a
  I.NameVar a -> varSym a

