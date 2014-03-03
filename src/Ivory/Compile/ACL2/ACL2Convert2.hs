-- | Compile CPS directly to ACL2.
module Ivory.Compile.ACL2.ACL2Convert2
  ( acl2Convert2
  ) where

import MonadLib

import Ivory.Compile.ACL2.ACL2
import Ivory.Compile.ACL2.ACL2Convert (showLit)
import Ivory.Compile.ACL2.CPS
import Ivory.Language.Syntax.AST (ExpOp (..))

type CN = StateT (Int, [Expr]) Id

acl2Convert2 :: [Proc ExpOp] -> [Expr]
acl2Convert2 procs = [opt1, opt2, opt3, assert, mutualRecursion $ callCont : defs, start]
  where
  ((), (n, defs)) = runId $ runStateT (0, []) $ mapM_ proc procs
  opt1 = call "set-irrelevant-formals-ok"     [lit "t"]
  opt2 = call "set-ignore-ok"                 [lit "t"]
  opt3 = call "set-bogus-mutual-recursion-ok" [lit "t"]
  start = defun "start" [] $ call "main" [nil]
  assert = defun "assert-cond" ["a", "b"] $ var "b"
  callCont = defun "call-cont" ["stack", "retval"] $ f [0 .. n - 1]
  f :: [Int] -> Expr
  f a = case a of
    [] -> nil
    a : b -> if' (equal (car stack) $ lit $ show a') (call a' [cdr stack, var "retval"]) (f b)
      where
      a' = "_cont_" ++ show a

proc :: Proc ExpOp -> CN ()
proc (Proc name args body) = do { body <- cont body; addFun name ("stack" : args) body }

genContName :: CN String
genContName = do
  (i, a) <- get
  set (i + 1, a)
  return $ "_cont_" ++ show i

addFun :: String -> [String] -> Expr -> CN ()
addFun name args body = do
  (i, a) <- get
  set (i, a ++ [defun name args body])

cont :: Cont ExpOp -> CN Expr
cont a = case a of
  Call f args ret -> do
    name <- genContName
    ret  <- cont ret
    addFun name ["stack", "retval"] ret
    return $ call f $ cons (lit $ show name) stack : map var args
  Halt         -> return nil
  Assert a b   -> do { b <- cont b; return $ call "assert-cond" [var a, b] }
  Assume a b   -> do { b <- cont b; return $ call "assert-cond" [var a, b] }
  If     a b c -> do { b <- cont b; c <- cont c; return $ if' (zip' $ var a) c b }
  Push   a b   -> do { b <- cont b; return $ let' [("stack", cons (var a) stack)] b }
  Let    a b c -> do
    c <- cont c
    case b of
      Var     b -> return $ let' [(a, var b)] c
      Literal b -> return $ let' [(a, lit $ showLit b)] c
      Pop       -> return $ let' [(a, car stack), ("stack", cdr stack)] c
      Intrinsic i args -> return $ let' [(a, b)] c
        where
        b = case i of
          ExpEq   _        -> if' (equal (arg 0) (arg 1)) 1 0
          ExpNeq  _        -> if' (not' (equal (arg 0) (arg 1))) 1 0
          ExpCond          -> if' (zip' (arg 0)) (arg 2) (arg 1)
          ExpGt   False _  -> if' (call ">"  [arg 0, arg 1]) 1 0
          ExpGt   True  _  -> if' (call ">"  [arg 0, arg 1]) 1 0
          ExpLt   False _  -> if' (call "<"  [arg 0, arg 1]) 1 0
          ExpLt   True  _  -> if' (call "<"  [arg 0, arg 1]) 1 0
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
        arg n = var $ args !! n
  Return (Just a) -> return $ call "call-cont" [stack, var a]
  Return Nothing  -> return $ call "call-cont" [stack, nil]

stack :: Expr
stack = var "stack"
