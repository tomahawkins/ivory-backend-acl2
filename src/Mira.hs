module Mira
  ( verifyAssertion
  , compile
  ) where

--import System.Environment

import Mira.CLL hiding (Expr)
import Mira.CPS hiding (Proc)
import Mira.CPSConvert
import Mira.Verify
import Mira.ACL2 (Expr)
import Mira.ACL2ConvertCPS

-- | Compile an CLL procedures to ACL2.
compile :: [Proc] -> [Expr]
compile cll = acl2ConvertCPS $ map (removeNullEffect . removeAsserts . commonSubExprElim) $ cpsConvert cll

