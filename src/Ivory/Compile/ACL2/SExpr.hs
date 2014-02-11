-- | S-Expressions for ACL2 generation.
module Ivory.Compile.ACL2.SExpr
  ( SExpr (..)
  ) where

data SExpr
  = SV String
  | SA [SExpr]

instance Show SExpr where
  show a = case a of
    SV a -> a ++ "\n"
    SA args -> "( " ++ indent (concatMap show args) ++ ")\n"
      where
      indent = drop 2 . unlines . map ("  " ++) . lines

