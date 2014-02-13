-- | A CPS IR.
module Ivory.Compile.ACL2.CPS
  ( Proc      (..)
  , SValue    (..)
  , BValue    (..)
  , Intrinsic (..)
  , Cont      (..)
  ) where

import qualified Ivory.Language.Syntax.AST as I
import Ivory.Language.Syntax.Names

-- | A procedure is a name, a list of arguments, and its continuation (body).
data Proc = Proc Sym [Sym] Cont deriving Show

-- | Small values are either variables or constants.
data SValue
  = Sym Sym             -- ^ A variable reference.
  | Literal I.Literal   -- ^ A literal constant.
  | ReturnValue         -- ^ The return value of a function.
  deriving Show

-- | Big values are more complicated expressions used in let binding.
data BValue
  = Intrinsic   Intrinsic [SValue]  -- ^ Apply an intrinsic to a list of arguments.
  | Deref       SValue              -- ^ Dereference a pointer.
  | SValue      SValue              -- ^ An SValue.
  deriving Show

-- | Intrinsics.
data Intrinsic
  = IntrinsicOp I.ExpOp        -- ^ At this point, intrinsics are the Ivory operators.
  deriving Show

-- | Continuations.
data Cont
  = Call    Sym [SValue]       -- ^ Function call, given the function name and arguments.
  | Push    Cont Cont          -- ^ Push a continuation onto the stack.
  | Pop     Cont               -- ^ Pop a continuation off the stack and throw it away. 
  | Return  (Maybe SValue)     -- ^ Pop a continuation off the stack and execute it.  Saves the return value to the ReturnValue register.
  | Let     Sym BValue Cont    -- ^ Brings a new variable into scope and assigns it a value.
  | If      SValue Cont Cont   -- ^ Conditionally follow one continuation or another.
  | Assert  SValue Cont        -- ^ Assert a value and continue.
  | Assume  SValue Cont        -- ^ State an assumption and continue.
  | Store   SValue SValue Cont -- ^ Store a value and continue.
  | Assign  Sym SValue Cont    -- ^ Assign a value and continue.
  | Forever Cont               -- ^ Loop forever on this continuation.
  | Loop    Sym SValue Bool SValue Cont Cont  -- ^ Loop a fixed number of times with a looping variable.
  | Halt                       -- ^ End the program or loop.
  deriving Show

{-

Questions and Ideas:

Q: How should continuations be represented in ACL2?

One thought is to collect all the pushed continuations and turn them into functions.
To be compliant with ACL2's restriction on first class functions, a Push operation
would then push a corresponding code, not the function, onto the continuation stack.
On a Return, a code would be popped of the stack and appropriate continuation would be called.

Compiled Ivory procedures would take the continuation stack as an extra argument.

Branches on If statements would also be turned into continuation functions.
The If would then select and call the appropriate continuation.

Or perhaps, every continuation gets its own continuation function. 


Q: How to handle Ivory state in ACL2 (Store, Assign, etc)?

Since CPS turns everything into a tail call, I think state can be 
managed by passing the global state into every compiled Ivory function
and continuation function.


Q: How to handle loops in ACL2?

Fixed duration Loops can be rewritten Forever loops with conditional breaks.
Is it possible rewrite Forever loops into a combination of CPS tricks?


Q: How are assertions verified?

Are programs rewritten (truncated) to assertion point and verified independently?
Would ACL2 be smart enough to tie this information back to the larger program?

Or would the compiled programs return all the assertions collect throughout the execution,
which ACL2 would verify independently?  But this would still happen under one defthm,
so I'm not sure how the various assertions would serve as lemmas for larger proofs.


Q: Is it possible to do CPS interpretation in ACL2?

Instead of direct compilation to ACL2, would it be better use an interpreter
approach, i.e.  write and CPS IR interpreter in ACL2, then feed it a generated CPS IR program?

How could you write an CPS interpreter in ACL2, which would clearly be nonterminating? 
Force it to halt after a million instructions?

Would this make verification any easier?


Q: Is CPS appropriate?

CPS is a good IR for optimization and compiling to assembly.
Is it appropriate for verification or would another IR be a better choice?

-}


