-- | Convert CLL to CPS.
module Mira.CPSConvert
  ( cpsConvert
  ) where

import Data.List (delete)
import MonadLib

import qualified Mira.CLL as C
import Mira.CPS
import Mira.Expr (Expr, Intrinsic (Add, Sub, Le, Ge))
import qualified Mira.Expr as E

cpsConvert :: [C.Proc] -> [Proc]
cpsConvert = snd . snd . runId . runStateT (0, []) . mapM cpsConvertProc

type CPS = StateT (Int, [Proc]) Id

addProc :: Var -> [Var] -> Maybe (Expr) -> Cont -> CPS ()
addProc fun args measure cont = do
  (i, procs) <- get
  set (i, procs ++ [Proc fun args measure cont])

cpsConvertProc :: C.Proc -> CPS ()
cpsConvertProc (C.Proc fun args measure body) = do
  cont <- cpsStmts body Halt
  addProc fun args measure cont

genVar :: CPS Var
genVar = do
  (i, p) <- get
  set (i + 1, p)
  return $ "_cps" ++ show i

cpsStmts :: [C.Stmt] -> Cont -> CPS Cont
cpsStmts a cont = case a of
  [] -> return cont
  a : b -> do
    cont <- cpsStmts b cont
    case a of
      C.Block a -> cpsStmts a cont
      C.If a b c -> do
        f <- genVar
        let args = contFreeVars cont
        addProc f args Nothing cont
        b <- cpsStmts b $ Call f args Nothing
        c <- cpsStmts c $ Call f args Nothing
        cpsExpr a $ \ a -> return $ If a b c
      C.Return (Just a) -> cpsExpr a $ \ a -> return $ Return $ Just a  -- This ignores cont (the rest of the function).  Is this ok?
      C.Return Nothing  -> return $ Return Nothing  -- Again, ignores cont.
      C.Assert a -> cpsExpr a $ \ a -> return $ Assert a cont
      C.Assume a -> cpsExpr a $ \ a -> return $ Assume a cont
      C.Let    a b -> cpsExpr b $ \ b -> return $ Let   a (Var   b) cont
      C.Store  a b -> case a of
        C.ArrayIndex  a i -> cpsExpr a $ \ a -> cpsExpr i $ \ i -> cpsExpr b $ \ b -> return $ Store (SArrayIndex  a i) b cont
        C.StructIndex a i -> cpsExpr a $ \ a ->                    cpsExpr b $ \ b -> return $ Store (SStructField a i) b cont
        a                 -> cpsExpr a $ \ a ->                    cpsExpr b $ \ b -> return $ Store (SRef a) b cont
      C.Call Nothing fun args -> f [] args
        where
        --f :: [Var] -> [C.Expr i] -> CPS i (Cont i)
        f args a = case a of
          [] -> return $ Call fun args $ Just cont
          a : b -> cpsExpr a $ \ a -> f (args ++ [a]) b
      C.Call (Just result) fun args -> f [] args
        where
        --f :: [Var] -> [C.Expr i] -> CPS i (Cont i)
        f args a = case a of
          [] -> return $ Call fun args $ Just $ Let result (Var "retval") cont
          a : b -> cpsExpr a $ \ a -> f (args ++ [a]) b

      -- XXX Need to add a check to ensure loop body doesn't have any return statements.
      C.Loop i init incr to body -> cpsExpr init $ \ init -> cpsExpr to $ \ to -> do
        body <- cpsStmts body Halt
        let args' = delete i $ delete to $ contFreeVars body
            args  = i : to : args'
        fun  <- genVar
        test <- genVar
        one  <- genVar
        addProc fun args
          (Just $ E.Intrinsic Add [E.Literal $ LitInteger 1, E.Intrinsic Sub (if incr then [E.Var to, E.Var i] else [E.Var i, E.Var to])]) $
          Let test (Intrinsic (if incr then Le else Ge) [i, to]) $ If test (replaceCont (f fun i args one) body) cont
        return $ Call fun (init : to : args') $ Just cont
        where
        -- Replace Halt with recursive call.
        f fun i args one a = case a of
          Halt -> Just $ Let one 1 $ Let i (Intrinsic (if incr then Add else Sub) [i, one]) $ Call fun args Nothing
          _    -> Nothing

cpsExprs :: [C.Expr] -> ([Var] -> CPS Cont) -> CPS Cont
cpsExprs a k = case a of
  [] -> k []
  a : b -> cpsExpr a $ \ a -> cpsExprs b $ \ b -> k $ a : b
  
cpsExpr :: C.Expr -> (Var -> CPS Cont) -> CPS Cont
cpsExpr a k = case a of
  C.Var a -> k a
  C.Literal a -> do
    v <- genVar
    cont <- k v
    return $ Let v (Literal a) cont
  C.Deref a -> do
    v <- genVar
    cont <- k v
    cpsExpr a $ \ a -> return $ Let v (Deref a) cont
  C.Alloc -> do
    v <- genVar
    cont <- k v
    return $ Let v Alloc cont
  C.Array items -> do
    array <- genVar
    cont <- k array
    cpsExprs items $ \ items -> return $ Let array (Array items) cont
  C.Struct fields -> do
    struct <- genVar
    cont <- k struct
    cpsExprs (snd $ unzip fields) $ \ values -> return $ Let struct (Struct $ zip (fst $ unzip fields) values) cont
  C.ArrayIndex a b -> do
    v <- genVar
    cont <- k v
    cpsExpr a $ \ a -> cpsExpr b $ \ b -> return $ Let v (ArrayIndex a b) cont
  C.StructIndex a b -> do
    v <- genVar
    cont <- k v
    cpsExpr a $ \ a -> return $ Let v (StructIndex a b) cont
  C.Intrinsic op args -> f args []
    where
    --f :: [C.Expr] -> [Var] -> CPS (Cont i)
    f argsE argsV = case argsE of
      [] -> do
        v <- genVar
        cont <- k v
        return $ Let v (Intrinsic op argsV) cont
      a : b -> cpsExpr a $ \ a -> f b (argsV ++ [a])

