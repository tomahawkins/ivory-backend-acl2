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

import Mira.CPS (Literal (..), Var)
import Mira.Intrinsics

data Proc = Proc Var [Var] [Stmt]

instance Show Proc where
  show (Proc name args body) = printf "%s(%s)\n%s\n" name (intercalate ", " args) (indent $ concatMap show body)

data Stmt
  = Call   (Maybe Var) Var [Expr]
  | If     Expr [Stmt] [Stmt]
  | Return (Maybe Expr)
  | Assert Expr
  | Assume Expr
  | Let    Var Expr
  | Loop   Var Expr Bool Expr [Stmt]

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
    Loop    a b c d e    -> printf "for (%s = %s; %s %s %s; %s%s)\n%s\n" a (show b) a (if c then "<=" else ">=") (show d) a (if c then "++" else "--") (indent $ concatMap show e)

indent :: String -> String
indent = intercalate "\n" . map ("\t" ++) . lines

data Expr
  = Var       Var
  | Lit       Literal
  | Intrinsic Intrinsic [Expr]

instance Show Expr where
  show a = case a of
    Var a -> a
    Lit a -> show a
    Intrinsic a args -> printf "%s(%s)" (show a) (intercalate ", " $ map show args)

