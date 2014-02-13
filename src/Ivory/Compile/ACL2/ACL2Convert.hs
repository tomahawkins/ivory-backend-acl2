-- | Compile CPS to ACL2.
module Ivory.Compile.ACL2.ACL2Convert
  ( acl2Convert
  ) where

import qualified Ivory.Compile.ACL2.ACL2 as A
import Ivory.Compile.ACL2.CPS
import qualified Ivory.Language.Syntax.AST as I
import Ivory.Language.Syntax.Names

acl2Convert :: Proc -> A.Expr
acl2Convert (Proc name args body) = A.Defun name args $ acl2Cont (zip args $ map A.Var args) body

acl2Cont :: [(Sym, A.Expr)] -> Cont -> A.Expr
acl2Cont env a = case a of
  Let     a b c -> A.Let' [(a, acl2BValue b)] $ acl2Cont ((a, acl2BValue b) : env) c
  --Let     a b c -> A.Cons (A.Lit "0") $ A.Cons (A.Cons (A.Lit $ show a) (acl2BValue b)) $ acl2Cont c  --A.Let' [(a, acl2BValue b)] $ acl2Cont ((a, acl2BValue b) : env) c
  --_ -> A.Lit "nil"
  a -> error $ "Continuation not supported yet: " ++ show a
{-
  = Call    Sym [SValue]       -- ^ Function call.
  | Push    Cont Cont          -- ^ Push a continuation onto the stack.
  | Pop     Cont               -- ^ Pop a continuation off the stack and throw it away. 
  | Return  (Maybe SValue)     -- ^ Pop a continuation off the stack and execute it.  Return value saved to ReturnValue.
  | Let     Sym BValue Cont
  | If      SValue Cont Cont
  | Halt
  | Assert  SValue Cont
  | Assume  SValue Cont
  | Store   SValue SValue Cont
  | Assign  Sym SValue Cont
  | Forever Cont
  | Loop    Sym SValue Incr SValue Cont Cont
  -}

acl2BValue :: BValue -> A.Expr
acl2BValue a = case a of
  Intrinsic   a args -> acl2Intrinsic a $ map acl2SValue args
  SValue      a -> acl2SValue a
  Deref _   -> error "acl2: Deref not supported."

acl2SValue :: SValue -> A.Expr
acl2SValue a = case a of
  Sym a       -> A.Lit $ show a
  ReturnValue -> A.Lit $ show "retval"
  Literal a   -> case a of
    I.LitInteger a     -> A.Lit $ show a
    I.LitFloat   a     -> A.Lit $ show a
    I.LitDouble  a     -> A.Lit $ show a
    I.LitChar    a     -> A.Lit $ show a
    I.LitString  a     -> A.Lit $ show a
    I.LitBool    True  -> A.Lit "true"
    I.LitBool    False -> A.Lit "nil"
    I.LitNull          -> A.Lit "nil"

acl2Intrinsic :: Intrinsic -> [A.Expr] -> A.Expr
acl2Intrinsic (IntrinsicOp a) args = case (a, args) of
  (I.ExpEq _,        [a, b]) -> A.Equal a b
  (I.ExpNeq _,       [a, b]) -> A.Not $ A.Equal a b
  (I.ExpGt False _,  [a, b]) -> A.Gt  a b
  (I.ExpGt True  _,  [a, b]) -> A.Ge  a b
  (I.ExpLt False _,  [a, b]) -> A.Lt  a b
  (I.ExpLt True  _,  [a, b]) -> A.Le  a b
  (I.ExpAdd,         [a, b]) -> A.Add a b
  (I.ExpSub,         [a, b]) -> A.Sub a b
  (I.ExpNot,         [a]   ) -> A.Not a
  (I.ExpAnd,         [a, b]) -> A.And [a, b]
  (I.ExpOr,          [a, b]) -> A.Or  [a, b]
  (I.ExpCond,     [a, b, c]) -> A.If  a b c
  a -> error $ "Intrinsic not supported yet: " ++ show a

