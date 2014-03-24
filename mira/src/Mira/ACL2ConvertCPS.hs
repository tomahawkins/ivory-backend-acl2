-- | Compile CPS directly to ACL2.
module Mira.ACL2ConvertCPS
  ( acl2ConvertCPS
  ) where

import Data.Maybe (fromJust)
import MonadLib

import Mira.ACL2
import Mira.ACL2ConvertRTL (showLit)
import Mira.CPS
import Mira.Intrinsics
import Mira.RecTopoSort

type CN = StateT (Int, [Expr], [(String, Expr)]) Id

acl2ConvertCPS :: [Proc] -> [Expr]
acl2ConvertCPS procs = [opt1, opt2] ++ mutualRecGroups
  where
  ((), (n, funs, conts)) = runId $ runStateT (0, [], []) $ mapM_ proc procs
  opt1     = call "set-irrelevant-formals-ok"     [lit "t"]
  opt2     = call "set-ignore-ok"                 [lit "t"]
  assert   = defun "assert-cond" ["a", "b"] $ var "b"
  callCont = defun "call-cont" ["stack", "retval"] $ if' (consp stack) (f [0 .. n - 1]) retval
    where
    f :: [Int] -> Expr
    f a = case a of
      [] -> retval
      a : b -> if' (equal (car stack) $ lit $ show a') (let' [("stack", cdr stack)] $ fromJust (lookup a' conts)) (f b)
        where
        a' = "_cont_" ++ show a
  defuns = assert : callCont : funs

  defunNames :: [String]
  defunNames = [ name | Obj (Lit "defun" : Lit name : _) <- defuns ]

  def :: String -> Expr
  def name = case [ d | d@(Obj (Lit "defun" : Lit f : _)) <- defuns, f == name ] of
    [d] -> d
    _ -> error $ "Problem finding function: " ++ name

  -- XXX This is not strictly correct.  It blindingly grabs all names, ignoring names introduced with lets.
  callees :: Expr -> [Expr]
  callees = map def . filter (flip elem defunNames) . f
    where 
    f :: Expr -> [String]
    f a = case a of
      Obj a -> concatMap f a
      Lit a -> [a]
  
  mutualRec :: [Expr] -> Expr
  mutualRec a = case a of
    [a] -> a
    a   -> mutualRecursion a

  mutualRecGroups = map mutualRec $ recTopoSort callees defuns

proc :: Proc -> CN ()
proc (Proc name args body) = do
  body <- cont body
  addFun name ("stack" : args) $ if' (foldl (and') t $ map (integerp . var) args) body nil

addFun :: String -> [String] -> Expr -> CN ()
addFun name args body = do
  (i, f, c) <- get
  set (i, f ++ [defun name args body], c)

addCont :: Cont -> CN String
addCont body = do  -- stack and retval are args to continuation function.
  body <- cont body
  (i, f, c) <- get
  let name = "_cont_" ++ show i
  set (i + 1, f, c ++ [(name, body)])
  return name

cont :: Cont -> CN Expr
cont a = case a of
  Call f args (Just ret) -> do
    name <- addCont ret
    return $ call f $ cons (lit $ show name) stack : map var args
  Call f args Nothing -> return $ call f $ stack : map var args
  Halt         -> return nil
  Assert a b   -> do { b <- cont b; return $ call "assert-cond" [var a, b] }
  Assume a b   -> do { b <- cont b; return $ call "assert-cond" [var a, b] }
  If     a b c -> do { b <- cont b; c <- cont c; return $ if' (zip' $ var a) c b }
  Push   a b   -> do { b <- cont b; return $ let' [("stack", cons (var a) stack)] b }
  Let    a b c -> do
    c <- cont c
    case b of
      Var     b        -> return $ let' [(a, var b)] c
      Literal b        -> return $ let' [(a, lit $ showLit b)] c
      Pop              -> return $ let' [(a, car stack), ("stack", cdr stack)] c
      Intrinsic i args -> return $ let' [(a, intrinsicACL2 i (map var args !!))] c
  Return (Just a) -> return $ call "call-cont" [stack, var a]
  Return Nothing  -> return $ call "call-cont" [stack, nil]

stack  = var "stack"
retval = var "retval"

