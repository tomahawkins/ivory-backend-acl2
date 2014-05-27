-- | Compile CPS directly to ACL2.
module Mira.ACL2ConvertCPS
  ( acl2ConvertCPS
  ) where

import Data.Maybe (fromJust)
import MonadLib

import Mira.ACL2
import Mira.CPS
import Mira.Expr (intrinsicACL2, showLit)
import qualified Mira.Expr as E
import Mira.RecTopoSort

type CN = StateT (Int, [Expr], [(String, Expr)]) Id

acl2ConvertCPS :: FilePath -> [Proc] -> [Expr]
acl2ConvertCPS acl2Sources procs = opts ++ mutualRecGroups
  where
  ((), (n, funs, conts)) = runId $ runStateT (0, [], []) $ mapM_ proc procs
  opts =
    [ call "include-book" [string $ acl2Sources ++ "/books/ccg/ccg", lit ":ttags", obj [obj [lit ":ccg"]], lit ":load-compiled-file", nil]
    , call "set-termination-method" [lit ":ccg"]
    , call "set-irrelevant-formals-ok"     [lit "t"]
    , call "set-ignore-ok"                 [lit "t"]
    ]
  assert   = defun "assert-cond" ["a", "b"] $ var "b"
  callCont = defun "call-cont" ["stack", "heap", "retval"] $ if' (consp stack) (f [0 .. n - 1]) retval
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
proc (Proc name args measure body) = do
  body <- cont body
  case measure of
    Nothing -> do
      (i, f, c) <- get
      set (i, f ++ [defun name ("stack" : "heap" : args) (if' (foldl (and') t $ map (integerp . var) args) body nil)], c)
    Just m -> do 
      (i, f, c) <- get
      set (i, f ++ [defun' name ("stack" : "heap" : args) (exprACL2 m) (if' (foldl (and') t $ map (integerp . var) args) body nil)], c)

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
    return $ call f $ cons (lit $ show name) stack : heap : map var args
  Call f args Nothing -> return $ call f $ stack : heap : map var args
  Halt         -> return nil
  Assert a b   -> do { b <- cont b; return $ call "assert-cond" [var a, b] }
  Assume a b   -> do { b <- cont b; return $ call "assert-cond" [var a, b] }
  If     a b c -> do { b <- cont b; c <- cont c; return $ if' (zip' $ var a) c b }
  Push   a b   -> do { b <- cont b; return $ let' [("stack", cons (var a) stack)] b }
  Let    a b c -> do
    c <- cont c
    return $ let' b' c
    where
    b' = case b of
      Var         b    -> [(a, var b)]
      Alloc            -> [(a, len heap), ("heap", append heap $ cons nil nil)]
      Array       b    -> [(a, list [ len heap + fromIntegral i | i <- [0 .. length b - 1] ]),                               ("heap", append heap $ list $ map var b)]
      Struct      b    -> [(a, list [ cons (string n) (len heap + fromIntegral i) | (n, i) <- zip (fst $ unzip b) [0 ..] ]), ("heap", append heap $ list $ map var $ snd $ unzip b)]
      Deref       b    -> [(a, nth (var b) heap)]
      ArrayIndex  b c  -> [(a, nth (var c) (var b))]
      StructIndex b c  -> [(a, cdr $ assoc (string c) (var b))]
      Literal     b    -> [(a, lit $ showLit b)]
      Pop              -> [(a, car stack), ("stack", cdr stack)]
      Intrinsic i args -> [(a, intrinsicACL2 i (map var args !!))]
  Store  a b c -> do
    c <- cont c
    return $ let' [("heap", updateNth (var a) (var b) heap)] c
  Return (Just a) -> return $ call "call-cont" [stack, heap, var a]
  Return Nothing  -> return $ call "call-cont" [stack, heap, nil]

stack  = var "stack"
heap   = var "heap"
retval = var "retval"

exprACL2 :: E.Expr -> Expr
exprACL2 a = case a of
  E.Var a -> var a
  E.Literal a -> lit $ showLit a
  E.Intrinsic i args -> intrinsicACL2 i (map exprACL2 args !!)
  _ -> error $ "Unsupported expression in measure: " ++ show a

