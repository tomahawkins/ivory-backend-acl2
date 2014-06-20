module Mira.Verify
  ( verifyProcs
  ) where

import System.IO
import Text.Printf

import Mira.ACL2
import Mira.CLL hiding (Expr)

verifyProcs :: [Proc] -> IO Bool
verifyProcs p = mapM verifyProc p >>= return . and

verifyProc :: Proc -> IO Bool
verifyProc (Proc name args _ requires ensures body) = do
  printf "Verifying procedure %s ..." name
  hFlush stdout
  a <- check [thm t]
  printf "\n"
  return a
  where


