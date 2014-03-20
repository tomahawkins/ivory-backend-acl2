-- | Compile CPS directly to ACL2.
module Mira.ACL2ConvertCPS
  ( acl2ConvertCPS
  ) where

import Data.Maybe (fromJust)
import MonadLib

import Ivory.Language.Syntax.AST (ExpOp (..))

import Mira.ACL2
import Mira.ACL2ConvertRTL (showLit)
import Mira.CPS
import Mira.RecTopoSort

type CN = StateT (Int, [Expr], [(String, Expr)]) Id

acl2ConvertCPS :: [Proc ExpOp] -> [Expr]
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

proc :: Proc ExpOp -> CN ()
proc (Proc name args body) = do
  body <- cont body
  addFun name ("stack" : args) $ if' (foldl (and') t $ map (integerp . var) args) body nil

genContName :: CN String
genContName = do
  (i, f, c) <- get
  set (i + 1, f, c)
  return $ "_cont_" ++ show i

addFun :: String -> [String] -> Expr -> CN ()
addFun name args body = do
  (i, f, c) <- get
  set (i, f ++ [defun name args body], c)

addCont :: String -> Expr -> CN ()
addCont name body = do  -- stack and retval are free in body.
  (i, f, c) <- get
  set (i, f, c ++ [(name, body)])

cont :: Cont ExpOp -> CN Expr
cont a = case a of
  Call f args (Just ret) -> do
    name <- genContName
    ret  <- cont ret
    addCont name ret
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
        arg n = var $ args !! n
  Return (Just a) -> return $ call "call-cont" [stack, var a]
  Return Nothing  -> return $ call "call-cont" [stack, nil]

stack  = var "stack"
retval = var "retval"

