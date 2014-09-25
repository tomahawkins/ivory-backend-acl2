-- | CLL: A C Like Language.
module Ivory.Compile.ACL2.CLL
  ( Proc    (..)
  , Stmt    (..)
  , Expr    (..)
  , Literal (..)
  , Var
  ) where

import Data.List
import Text.Printf

import Ivory.Compile.ACL2.Expr

data Proc = Proc 
  { procName     :: Var
  , procArgs     :: [Var]
  , procMeasure  :: Maybe Expr
  , procRequires :: [Expr]
  , procEnsures  :: [Expr -> Expr]
  , procBody     :: [Stmt]
  }

instance Show Proc where
  show (Proc name args Nothing  _ _ body) = printf "%s(%s)\n%s\n" name (intercalate ", " args) (indent $ concatMap show body)
  show (Proc name args (Just m) _ _ body) = printf "%s(%s)\n\tmeasure %s\n%s\n" name (intercalate ", " args) (show m) (indent $ concatMap show body)

data Stmt
  = Call   (Maybe Var) Var [Expr]
  | If     Expr [Stmt] [Stmt]
  | Return (Maybe Expr)
  | Assert Expr
  | Assume Expr
  | Let    Var Expr
  | Alloc  Var
  | Store  Expr Expr
  | Loop   Var Expr Bool Expr [Stmt]
  | Block  [Stmt]
  | Null

instance Show Stmt where
  show a = case a of
    Call   Nothing a b  -> printf "%s(%s)\n" a (intercalate ", " $ map show b)
    Call   (Just c) a b -> printf "%s = %s(%s)\n" c a (intercalate ", " $ map show b)
    Return (Just a)     -> printf "return %s\n" $ show a
    Return Nothing      -> "return\n"
    If     a b c        -> printf "if (%s)\n" (show a) ++ indent (concatMap show b) ++ "\nelse\n" ++ indent (concatMap show c)
    Assert a            -> printf "assert %s\n" $ show a
    Assume a            -> printf "assume %s\n" $ show a
    Let    a b          -> printf "let %s = %s\n" a $ show b
    Alloc  a            -> printf "alloc %s\n" a
    Store  a b          -> printf "store %s = %s\n" (show a) (show b)
    Loop   a b c d e    -> printf "for (%s = %s; %s %s %s; %s%s)\n%s\n" a (show b) a (if c then "<=" else ">=") (show d) a (if c then "++" else "--") (indent $ concatMap show e)
    Block  a            -> concatMap show a
    Null                -> ""

indent :: String -> String
indent = intercalate "\n" . map ("\t" ++) . lines

