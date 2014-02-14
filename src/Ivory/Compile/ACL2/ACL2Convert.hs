-- | Compile CPS to ACL2.
module Ivory.Compile.ACL2.ACL2Convert
  ( acl2Convert
  ) where

import Ivory.Compile.ACL2.ACL2
import qualified Ivory.Compile.ACL2.RTL as R
import Ivory.Language.Syntax.AST (ExpOp (..))

acl2Convert :: R.Program ExpOp -> Expr
acl2Convert _ = Nil

{-
type CC = StateT [A.Expr] Id

acl2Cont :: Cont -> CC (Var, [Var])
acl2Cont a = case a of
-}
  {-
  Let  a b c -> do
    (c, cArgs) <- acl2Cont c
    A.Let' [(a, acl2BValue b)] $ acl2Cont ((a, acl2BValue b) : env) c
    -}
  --Let     a b c -> A.Cons (A.Lit "0") $ A.Cons (A.Cons (A.Lit $ show a) (acl2BValue b)) $ acl2Cont c  --A.Let' [(a, acl2BValue b)] $ acl2Cont ((a, acl2BValue b) : env) c
  --_ -> A.Lit "nil"
  --a -> error $ "Continuation not supported yet: " ++ show a
  {-
  = Call    Var [SValue]       -- ^ Function call.
  | Push    Cont Cont          -- ^ Push a continuation onto the stack.
  | Pop     Cont               -- ^ Pop a continuation off the stack and throw it away. 
  | Return  (Maybe SValue)     -- ^ Pop a continuation off the stack and execute it.  Return value saved to ReturnValue.
  | Let     Var BValue Cont
  | If      SValue Cont Cont
  | Halt
  | Assert  SValue Cont
  | Assume  SValue Cont
  | Store   SValue SValue Cont
  | Assign  Var SValue Cont
  | Forever Cont
  | Loop    Var SValue Incr SValue Cont Cont
  -}

{-
acl2BValue :: BValue -> A.Expr
acl2BValue a = case a of
  SValue      a -> acl2SValue a
  Intrinsic   a args -> acl2Intrinsic a $ map acl2SValue args
  Literal     a -> case a of
    I.LitInteger a     -> A.Lit $ show a
    I.LitFloat   a     -> A.Lit $ show a
    I.LitDouble  a     -> A.Lit $ show a
    I.LitChar    a     -> A.Lit $ show a
    I.LitString  a     -> A.Lit $ show a
    I.LitBool    True  -> A.Lit "true"
    I.LitBool    False -> A.Lit "nil"
    I.LitNull          -> A.Lit "nil"

acl2SValue :: SValue -> A.Expr
acl2SValue a = case a of
  Var a       -> A.Var a
  ReturnValue -> A.Var "retval"

acl2Intrinsic :: I.ExpOp -> [A.Expr] -> A.Expr
acl2Intrinsic a args = case (a, args) of
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

-}
