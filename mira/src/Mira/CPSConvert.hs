-- | Convert CLL to CPS.
module Mira.CPSConvert
  ( cpsConvert
  ) where

import Data.List (delete)
import MonadLib

import qualified Mira.CLL as C
import Mira.CPS
import Mira.Intrinsics

cpsConvert :: Intrinsics i => [C.Proc i] -> [Proc i]
cpsConvert = snd . snd . runId . runStateT (0, []) . mapM cpsConvertProc

type CPS i = StateT (Int, [Proc i]) Id

addProc :: Var -> [Var] -> Cont i -> CPS i ()
addProc fun args cont = do
  (i, procs) <- get
  set (i, procs ++ [Proc fun args cont])

cpsConvertProc :: Intrinsics i => C.Proc i -> CPS i ()
cpsConvertProc (C.Proc fun args body) = do
  cont <- cpsStmts body Halt
  addProc fun args cont

genVar :: CPS i Var
genVar = do
  (i, p) <- get
  set (i + 1, p)
  return $ "_cps" ++ show i

cpsStmts :: Intrinsics i => [C.Stmt i] -> Cont i -> CPS i (Cont i)
cpsStmts a cont = case a of
  [] -> return cont
  a : b -> do
    cont <- cpsStmts b cont
    case a of
      C.If a b c -> do
        f <- genVar
        let args = contFreeVars cont
        addProc f args cont
        b <- cpsStmts b $ Call f args Nothing
        c <- cpsStmts c $ Call f args Nothing
        cpsExpr a $ \ a -> return $ If a b c
      C.Return (Just a) -> cpsExpr a $ \ a -> return $ Return $ Just a  -- This ignores cont (the rest of the function).  Is this ok?
      C.Return Nothing  -> return $ Return Nothing  -- Again, ignores cont.
      C.Assert a -> cpsExpr a $ \ a -> return $ Assert a cont
      C.Assume a -> cpsExpr a $ \ a -> return $ Assume a cont
      C.Let    a b -> cpsExpr b $ \ b -> return $ Let a (Var b) cont
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
        addProc fun args $ Let test (Intrinsic (if incr then intrinsicLE else intrinsicGE) [i, to]) $ If test (replaceCont (f fun i args one) body) cont
        return $ Call fun (init : to : args') $ Just cont
        where
        -- Replace Halt with recursive call.
        f fun i args one a = case a of
          Halt -> Just $ Let one 0 $ Let i (Intrinsic (if incr then intrinsicAdd else intrinsicSub) [i, one]) $ Call fun args Nothing
          _    -> Nothing

cpsExpr :: C.Expr i -> (Var -> CPS i (Cont i)) -> CPS i (Cont i)
cpsExpr a k = case a of
  C.Var a -> k a
  C.Lit a -> do
    v <- genVar
    cont <- k v
    return $ Let v (Literal a) cont
  C.Intrinsic op args -> f args []
    where
    --f :: [C.Expr] -> [Var] -> CPS (Cont i)
    f argsE argsV = case argsE of
      [] -> do
        v <- genVar
        cont <- k v
        return $ Let v (Intrinsic op argsV) cont
      a : b -> cpsExpr a $ \ a -> f b (argsV ++ [a])

