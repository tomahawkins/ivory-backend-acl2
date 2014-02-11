-- | A CPS IR.
module Ivory.Compile.ACL2.CPS
  ( Proc      (..)
  , SValue    (..)
  , BValue    (..)
  , Intrinsic (..)
  , Cont      (..)
  , Incr      (..)
  ) where

import qualified Ivory.Language.Syntax.AST as I
import Ivory.Language.Syntax.Names

-- | A procedure is a name, a list of arguments, and its continuation (body).
data Proc = Proc Sym [Sym] Cont deriving Show

-- | Small values are either variables or constants.
data SValue
  = Sym Sym
  | Literal I.Literal
  | ReturnValue         -- ^ The return value of a function.
  deriving Show

-- | Big values.
data BValue
  = Intrinsic   Intrinsic [SValue]
  | Deref       SValue
  | SValue      SValue
  deriving Show

-- | Intrinsics.
data Intrinsic
  = IntrinsicOp I.ExpOp
  deriving Show

-- | Continuations.
data Cont
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
  deriving Show

-- | Loop increment/decrement direction.
data Incr = Incr | Decr deriving Show

