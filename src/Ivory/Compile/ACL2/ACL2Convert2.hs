-- | Compile CPS directly to ACL2.
module Ivory.Compile.ACL2.ACL2Convert2
  ( acl2Convert2
  ) where

import Ivory.Compile.ACL2.ACL2
import Ivory.Compile.ACL2.CPS
import Ivory.Language.Syntax.AST (ExpOp (..))

acl2Convert2 :: [Proc ExpOp] -> [Expr]
acl2Convert2 procs = [mutualRecursion $ map proc procs]

proc :: Proc ExpOp -> Expr
proc (Proc name args body) = defun name ("stack" : args) nil

