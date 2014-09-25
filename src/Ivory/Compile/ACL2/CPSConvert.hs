-- | Convert CLL to CPS.
module Ivory.Compile.ACL2.CPSConvert
  ( cpsConvert
  ) where

import Data.List (delete)
import MonadLib

import qualified Ivory.Compile.ACL2.CLL as C
import Ivory.Compile.ACL2.CPS
import Ivory.Compile.ACL2.Expr (Expr, Intrinsic (Add, Sub, Le, Ge))
import qualified Ivory.Compile.ACL2.Expr as E

cpsConvert :: [C.Proc] -> [Proc]
cpsConvert = snd . snd . runId . runStateT (0, []) . mapM cpsConvertProc

type CPS = StateT (Int, [Proc]) Id

addProc :: Var -> [Var] -> Maybe (Expr) -> Cont -> CPS ()
addProc fun args measure cont = do
  (i, procs) <- get
  set (i, procs ++ [Proc fun args measure cont])

cpsConvertProc :: C.Proc -> CPS ()
cpsConvertProc (C.Proc fun args measure _ _ body) = do
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
      C.Null -> return cont
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
      C.Assert a   -> cpsExpr a $ \ a -> return $ Assert a cont
      C.Assume a   -> cpsExpr a $ \ a -> return $ Assume a cont
      C.Let    a b -> cpsExpr b $ \ b -> return $ Let a (Var b) cont
      C.Alloc  a   -> return $ Let a Alloc cont
      C.Store  a b -> cpsExpr a $ \ a -> cpsExpr b $ \ b -> return $ Store a b cont
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
          Let test (Intrinsic (if incr then Le else Ge) [i, to]) $ If test (replaceCont (f fun i args one) body) $ Return Nothing
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
cpsExpr a k = do
  v <- genVar
  cont <- k v
  let f a = return $ Let v a cont
  case a of
    C.Var a     -> k a
    C.Literal a -> f $ Literal a
    C.Deref   a -> cpsExpr a $ \ a -> f $ Deref a
    --C.Alloc     -> f Alloc
    C.Array  items  -> cpsExprs items $ \ items -> f $ Array items
    C.Struct fields -> cpsExprs (snd $ unzip fields) $ \ values -> f $ Struct $ zip (fst $ unzip fields) values
    C.ArrayIndex  a b -> cpsExpr a $ \ a -> cpsExpr b $ \ b -> f $ ArrayIndex  a b
    C.StructIndex a b -> cpsExpr a $ \ a ->                    f $ StructIndex a b
    C.Intrinsic op args -> f1 args []
      where
      --f :: [C.Expr] -> [Var] -> CPS (Cont i)
      f1 argsE argsV = case argsE of
        [] -> f $ Intrinsic op argsV
        a : b -> cpsExpr a $ \ a -> f1 b (argsV ++ [a])

