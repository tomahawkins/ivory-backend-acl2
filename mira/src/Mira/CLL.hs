-- | CLL: A C Like Language.
module Mira.CLL
  ( Proc    (..)
  , Stmt    (..)
  , Expr    (..)
  , Literal (..)
  , Var
  ) where

import Data.List
import Text.Printf

import Mira.Expr

data Proc = Proc Var [Var] (Maybe Expr) [Stmt]

instance Show Proc where
  show (Proc name args Nothing  body) = printf "%s(%s)\n%s\n" name (intercalate ", " args) (indent $ concatMap show body)
  show (Proc name args (Just m) body) = printf "%s(%s)\n\tmeasure %s\n%s\n" name (intercalate ", " args) (show m) (indent $ concatMap show body)

data Stmt
  = Call   (Maybe Var) Var [Expr]
  | If     Expr [Stmt] [Stmt]
  | Return (Maybe Expr)
  | Assert Expr
  | Assume Expr
  | Let    Var Expr
  | Loop   Var Expr Bool Expr [Stmt]
  | Store  Var Expr

instance Show Stmt where
  show a = case a of
    Call    Nothing a b  -> printf "%s(%s)\n" a (intercalate ", " $ map show b)
    Call    (Just c) a b -> printf "%s = %s(%s)\n" c a (intercalate ", " $ map show b)
    Return  (Just a)     -> printf "return %s\n" $ show a
    Return  Nothing      -> "return\n"
    If      a b c        -> printf "if (%s)\n" (show a) ++ indent (concatMap show b) ++ "\nelse\n" ++ indent (concatMap show c)
    Assert  a            -> printf "assert %s\n" $ show a
    Assume  a            -> printf "assume %s\n" $ show a
    Let     a b          -> printf "%s = %s\n" a $ show b
    Store   a b          -> printf "%s = %s\n" a $ show b
    Loop    a b c d e    -> printf "for (%s = %s; %s %s %s; %s%s)\n%s\n" a (show b) a (if c then "<=" else ">=") (show d) a (if c then "++" else "--") (indent $ concatMap show e)

indent :: String -> String
indent = intercalate "\n" . map ("\t" ++) . lines

