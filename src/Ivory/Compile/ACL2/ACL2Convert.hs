-- | Compile CPS to ACL2.
module Ivory.Compile.ACL2.ACL2Convert
  ( acl2Convert
  ) where

import MonadLib

import Language.ACL2

import Ivory.Compile.ACL2.CPS
import Ivory.Compile.ACL2.Expr (intrinsicACL2, showLit)
import qualified Ivory.Compile.ACL2.Expr as E
import Ivory.Compile.ACL2.RecTopoSort

type CN = StateT (Int, [Expr]) Id

acl2Convert :: [Proc] -> [Expr]
acl2Convert procs = opts ++ mutualRecGroups
  where
  ((), (_, funs)) = runId $ runStateT (0, []) $ mapM_ proc procs
  opts =
    -- [ call "include-book" [string $ acl2Sources ++ "/books/ccg/ccg", lit ":ttags", obj [obj [lit ":ccg"]], lit ":load-compiled-file", nil]
    -- , call "set-termination-method" [lit ":ccg"]
    [ call "set-irrelevant-formals-ok"     [lit "t"]
    , call "set-ignore-ok"                 [lit "t"]
    ]
  assert   = defun "assert-cond" ["a", "b"] $ var "b"
  defuns = assert : funs

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
      (i, f) <- get
      set (i, f ++ [defun  name ("heap" : args)              (if' (foldl (and') t $ map (integerp . var) args) body $ cons nil nil)])
    Just m -> do 
      (i, f) <- get
      set (i, f ++ [defun' name ("heap" : args) (exprACL2 m) (if' (foldl (and') t $ map (integerp . var) args) body $ cons nil nil)])

genvar :: CN String
genvar = do
  (i, f) <- get
  set (i + 1, f)
  return $ "__" ++ show i

cont :: Cont -> CN Expr
cont a = case a of
  Call f args (Just rest) -> do
    ret <- genvar
    rest <- cont rest
    return $ let' [(ret, call f (heap : map var args)), ("heap", car $ var ret), ("retval", cdr $ var ret)] rest
  Call f args Nothing -> return $ call f $ heap : map var args
  Halt         -> return $ cons heap nil
  Assert a b   -> do { b <- cont b; return $ call "assert-cond" [var a, b] }
  Assume a b   -> do { b <- cont b; return $ call "assert-cond" [var a, b] }
  If     a b c -> do { b <- cont b; c <- cont c; return $ if' (zip' $ var a) c b }
  Push   _ _   -> error "Unsupported: Push"
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
      Pop              -> error "Unsupported: Pop"
      Intrinsic i args -> [(a, intrinsicACL2 i (map var args !!))]
  Store  a b c -> do
    c <- cont c
    return $ let' [("heap", updateNth (var a) (var b) heap)] c
  Return (Just a) -> return $ cons heap $ var a
  Return Nothing  -> return $ cons heap nil

heap   = var "heap"

exprACL2 :: E.Expr -> Expr
exprACL2 a = case a of
  E.Var a -> var a
  E.Literal a -> lit $ showLit a
  E.Intrinsic i args -> intrinsicACL2 i (map exprACL2 args !!)
  _ -> error $ "Unsupported expression in measure: " ++ show a

