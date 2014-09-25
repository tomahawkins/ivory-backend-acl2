module Ivory.Compile.ACL2.Compile
  ( compile
  ) where

import Language.ACL2 as ACL2

import Ivory.Compile.ACL2.CLL as CLL
import Ivory.Compile.ACL2.CPS hiding (Proc)
import Ivory.Compile.ACL2.CPSConvert
import Ivory.Compile.ACL2.ACL2Convert

-- | Compile CLL procedures to ACL2.
compile :: [CLL.Proc] -> [ACL2.Expr]
compile cll = acl2Convert $ map (removeNullEffect . removeAsserts . commonSubExprElim) $ cpsConvert cll

