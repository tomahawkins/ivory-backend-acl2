module Mira.Verify
  ( verifyProcs
  ) where

import System.IO
import Text.Printf

import Mira.ACL2
import Mira.CLL hiding (Expr)
import qualified Mira.CLL as C
import Mira.Expr (exprACL2)

verifyProcs :: [Proc] -> IO Bool
verifyProcs p = mapM verifyProc p >>= return . and

verifyProc :: Proc -> IO Bool
verifyProc (Proc name args _ requires ensures body) = do
  printf "Verifying procedure %s ..." name
  hFlush stdout
  a <- check $ (:[]) $ thm $ implies (foldl and' t $ map exprACL2 requires) t
  printf "\n"
  return a
  where

