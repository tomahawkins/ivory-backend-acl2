module Mira.Verify
  ( verifyAssertion
  ) where

--import System.IO
--import Text.Printf

import Mira.ACL2
import qualified Mira.CLL as C
import Mira.CPS
import Mira.CPSConvert
import Mira.Expr (exprACL2)

{-
verifyProcs :: [Proc] -> IO Bool
verifyProcs p = mapM verifyProc p >>= return . and

verifyProc :: Proc -> IO Bool
verifyProc (Proc name args _ requires ensures body) = do
  printf "Verifying procedure %s ..." name
  hFlush stdout
  a <- verifyAssertions 0 (implies (foldl and' t $ map bool requires)) body
  printf "\n"
  return a

bool :: C.Expr -> Expr
bool = not' . zip' . exprACL2

checkThm :: Expr -> IO Bool
checkThm a = check [thm a]

verifyAssertions :: Int -> (Expr -> Expr) -> [Stmt] -> IO Bool
verifyAssertions nextId body stmts = case stmts of
  [] -> return True
  stmt : stmts -> case stmt of
    Assert a -> do
      pass <- checkThm $ body a'
      if pass
        then verifyAssertions nextId (body . implies a') stmts
        else do
          putStrLn $ "Assertion failed: " ++ show a
          verifyAssertions nextId body stmts
          return False
      where
      a' = bool a
    Assume a -> verifyAssertions nextId (body . implies (bool a)) stmts
    Null     -> verifyAssertions nextId body stmts
    Return _ -> verifyAssertions nextId body stmts
    Block  a -> verifyAssertions nextId body $ a ++ stmts
    _ -> error $ "Unsupported statement: " ++ show stmt

    {-
    Call   Nothing a b  -> printf "%s(%s)\n" a (intercalate ", " $ map show b)
    Call   (Just c) a b -> printf "%s = %s(%s)\n" c a (intercalate ", " $ map show b)
    If     a b c        -> printf "if (%s)\n" (show a) ++ indent (concatMap show b) ++ "\nelse\n" ++ indent (concatMap show c)
    Let    a b          -> printf "let %s = %s\n" a $ show b
    Store  a b          -> printf "store %s = %s\n" (show a) (show b)
    Loop   a b c d e    -> printf "for (%s = %s; %s %s %s; %s%s)\n%s\n" a (show b) a (if c then "<=" else ">=") (show d) a (if c then "++" else "--") (indent $ concatMap show e)
    -}
-}

verifyAssertion :: [C.Proc] -> IO Bool
verifyAssertion procs' = return True -- XXX
  where
  procs = cpsConvert procs'

  [Proc _ args _ body] = filter procHasMark procs

  procHasMark :: Proc -> Bool
  procHasMark (Proc _ _ _ a) = contHasMark a

  contHasMark :: Cont -> Bool
  contHasMark a = case a of
    Mark _ -> True
    Halt   -> False
    Call   _ _ (Just a) -> contHasMark a
    Call   _ _ Nothing -> False
    Return _     -> False
    Push   _ a   -> contHasMark a
    Let    _ _ a -> contHasMark a
    Store  _ _ a -> contHasMark a
    If     _ a b -> contHasMark a || contHasMark b
    Assert _ a   -> contHasMark a
    Assume _ a   -> contHasMark a

