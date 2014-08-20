module Mira
  ( compile
  ) where

--import System.Environment

import Mira.CLL as CLL
import Mira.CPS hiding (Proc)
import Mira.CPSConvert
import Mira.ACL2 as ACL2
import Mira.ACL2Convert

-- | Compile CLL procedures to ACL2.
compile :: [CLL.Proc] -> [ACL2.Expr]
compile cll = acl2Convert $ map (removeNullEffect . removeAsserts . commonSubExprElim) $ cpsConvert cll

