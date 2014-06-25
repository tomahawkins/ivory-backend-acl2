# Ivory ACL2 Backend

[Ivory](https://github.com/GaloisInc/ivory) is a C like DSL embedded in [Haskell](http://haskell.org)
for hard realtime embedded applications.  [Galois](http://corp.galois.com/) is currently
using Ivory to build a [quadcopter autopilot](http://smaccmpilot.org/) for [DARPA](http://www.darpa.mil/)'s
[HACMS](http://www.darpa.mil/Our_Work/I2O/Programs/High-Assurance_Cyber_Military_Systems_(HACMS).aspx) program.

This ivory-backend-acl2 library provides a means to compile Ivory programs to ACL2 for formal analysis.

# Installation

1. Install the [Haskell Platform](http://www.haskell.org/platform/).
1. Install the Ivory DSL:
   1. Clone the [ivory](https://github.com/GaloisInc/ivory) git repo.
   1. Build and install the ivory sub library:

      `$ cd <ivory-repo-directory>/ivory && cabal install`

1. Install Ivory's ACL2 backend:
   1. Clone the [ivory-backend-acl2](https://github.com/tomahawkins/ivory-backend-acl2) repo.
   1. Build and install the mira library:

      `$ cd <ivory-backend-acl2-directory>/mira && cabal install`

   1. Build and install the ivory-backend-acl2 library:

      `$ cd <ivory-backend-acl2-directory>/ivory-backend-acl2 && cabal install`

1. Install [ACL2](http://www.cs.utexas.edu/users/moore/acl2/) and set the __ACL2_SOURCES__
   environment variable to point to the installation location:

   `$ export ACL2_SOURCES=<path-to-acl2-sources>`

# An Example

In this example we will write a factorial function in Ivory, compile it to ACL2,
and then prove termination of the function.

We are going to create a file (Factorial.hs) to capture the program and run the verification.
First we need some pragmas to allow certain Haskell extensions leveraged by the Ivory language.
We also need to import the Ivory DSL and the ACL2 generator libraries:

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Ivory.Language
import Ivory.Compile.ACL2
```

Next, write a recursive factorial function in Ivory:

```haskell
factorial :: Def ('[Sint32] :-> Sint32)
factorial = proc "factorial" $ \ n -> body $
  ifte_ (n >? 1)
    (do n' <- call factorial $ n - 1
        ret $ n' * n)
    (do ret n)
```

Now we just need to put the factorial function into an Ivory module
and call verifyTermination:

```haskell
main :: IO ()
main = do
  pass <- verifyTermination $ package "factorial" $ incl factorial
  putStrLn $ if pass then "PASS" else "FAIL"
```

Finally, run the program to prove (or disprove) termination of the Ivory function:

```
$ runhaskell -W Factorial.hs
```

This should result in a `PASS` printed on stdout.

Next, let us change the function so it doesn't terminate.  This can be accomplished by
negating the condition in the if-then-else branch:

```
factorial = proc "factorial" $ \ n -> body $
  ifte_ (iNot $ n >? 1)
    ...
```

This time the verification returns a `FAIL`.

## Using the Generated ACL2

Now fix the factorial function and rerun the termination verification.
This will compile the Ivory factorial function to ACL2 in the file: __factorial-cps.lisp__.
The resulting ACL2 factorial function looks like this:

```lisp
( defun
  factorial
  ( stack
    heap
    var0
  )
  ...
)
```

You will notice that factorial takes 3 arguments, not just one.  These extra arguments
are the stack and heap used in the ACL2 representation of Ivory.  When calling an Ivory
function in ACL2, pass in __nil__ for the starting stack and heap.

Now that we have factorial in ACL2, let's prove a property about it.
Add the following theorem to the bottom of the file and run it through ACL2:

```lisp
(thm (equal (factorial nil nil 4) 24))
```

Running:
```
$ acl2 < factorial-cps.lisp
```

Results in:
```
Summary
Form:  ( THM ...)
Rules: ((:EXECUTABLE-COUNTERPART EQUAL)
        (:EXECUTABLE-COUNTERPART FACTORIAL))
Time:  0.00 seconds (prove: 0.00, print: 0.00, other: 0.00)
Prover steps counted:  6

Proof succeeded.
ACL2 !>Bye.
```

# A Closer Look at the Compilation Process

Running the above example produces the following files,
which are the various intermediate representations (IRs)
of the Ivory to ACL2 compiler flow:

1. __factorial.cll__

  The first step in the translation converts the
  [Ivory AST](https://github.com/GaloisInc/ivory/blob/master/ivory/src/Ivory/Language/Syntax/AST.hs)
  to a smaller, simpler form called
  [CLL](https://github.com/tomahawkins/ivory-backend-acl2/blob/master/mira/src/Mira/CLL.hs).
  This smaller language provides top level function definitions
  and Call, If, Return, Assert, Let, and Loop statements
  along with a host of [Expressions](https://github.com/tomahawkins/ivory-backend-acl2/blob/master/mira/src/Mira/Expr.hs).

1. __factorial.cps1__

  From CLL we then translate into a continuation passing style
  ([CPS](https://github.com/tomahawkins/ivory-backend-acl2/blob/master/mira/src/Mira/CPS.hs))
  form.  In CPS expression evaluation order is made explicit
  and all function calls become tail-calls.
  This CPS IR provides the following continuation types:  Halt, Call, Return, Let, If, and Assert.
  In addition, this IR also provides for explicit stack operations (Push, Pop).

1. __factorial.cps2__

  This translation stays within CPS, but makes stack operations
  explicit (Push, Pop) to save and restore variables across function calls.
  
1. __factorial-cps.lisp__

  At this point, the CPS form with explicit stack operations is converted
  into ACL2.  All continuations in the program are given a name and
  a single, recursive ACL2 function pull a continuation name off the stack
  and executes it.  The stack and a data structure capturing global state
  is threaded through all the ACL2 functions as arguments.

1. __factorial.rtl__

  The ACL2 backend of Ivory also has the ability to compile down to a
  lower form to prove arbitrary assertions of Ivory programs.  This
  form, called [RTL](https://github.com/tomahawkins/ivory-backend-acl2/blob/master/mira/src/Mira/RTL.hs),
  is an stack machine, and provides Branch, Jump, Push, Pop, and other
  assembly-like instructions.

1. __factorial-rtl.lisp__

  From RTL, the compiler generates ACL2 that provides both a machine
  model for the instructions of RTL, and the program compiled to run 
  on this machine.  Ivory assertions are captured as conditional
  branches around Fail instructions.  The ACL2 theorem prover
  attempts to prove that all Fail instructions in a program are
  unreachable.

# Ivory Language Coverage

The follow tables list Ivory features supported by the ACL2 backend.

## Ivory Statements

Name           | Description                                         | Implemented | Testcase
---------------|-----------------------------------------------------|-------------|----------
IfTE           | If-then-else.                                       | X           | factorialTests
Return         | Return from a function call with an optional value. | X           | factorialTests
Assert         | User assertions.                                    | X           | intrinsicTest
Assume         | User assumptions.                                   | X           | intrinsicTest
CompilerAssert | Compiler generated assertions.                      | X           | loopTest
Local          | Local variable introduction.                        | X           | factorialTests
Call           | Function calls.                                     | X           | factorialTests
Loop           | A loop over a fixed iteration.                      | X           | loopTest
Store          | A store operation to a variable, array, or struct.  | X           | loopTest
AllocRef       | Allocation reference.                               | X           | loopTest
Defef          | Pointer dereference.                                | X           | loopTest 
Assign         | Variable assignment.                                |             |          
Forever        | A forever loop.                                     |             |          
Break          | A break statement for a loop.                       |             |          
RefCopy        |                                                     |             |          

## Ivory Expressions

Name            | Description                                     | Implemented | Testcase
----------------|-------------------------------------------------|-------------|----------
ExpSym          | Symbols.                                        | X           | intrinsicTest
ExpVar          | Variables.                                      | X           | intrinsicTest
ExpLiteral      | Liternal constants.                             | X           | intrinsicTest
ExpExpOp        | Intrinsic (operator) application.               | X           | intrinsicTest
ExpIndex        | Array indexing.                                 | X           | structArrayTest
ExpLabel        | Structure label indexing.                       | X           | structArrayTest
ExpSafeCast     | Type casting.                                   | X           | arrayTest

## Ivory Intrinsics

Name              | Implemented | Testcase
------------------|-------------|---------
ExpEq             | X           | intrinsicTest
ExpNeq            | X           | intrinsicTest
ExpCond           | X           | intrinsicTest
ExpGt             | X           | intrinsicTest
ExpLt             | X           | intrinsicTest
ExpNot            | X           | intrinsicTest
ExpAnd            | X           | intrinsicTest
ExpOr             | X           | intrinsicTest
ExpMul            | X           | factorialTests
ExpMod            | X           | intrinsicTest
ExpAdd            | X           | intrinsicTest
ExpSub            | X           | intrinsicTest
ExpNegate         | X           | intrinsicTest
ExpAbs            | X           | intrinsicTest
ExpSignum         | X           | intrinsicTest
ExpDiv            |             |
ExpRecip          |             |
ExpFExp           |             |
ExpFSqrt          |             |
ExpFLog           |             |
ExpFPow           |             |
ExpFLogBase       |             |
ExpFSin           |             |
ExpFTan           |             |
ExpFCos           |             |
ExpFAsin          |             |
ExpFAtan          |             |
ExpFAcos          |             |
ExpFSinh          |             |
ExpFTanh          |             |
ExpFCosh          |             |
ExpFAsinh         |             |
ExpFAtanh         |             |
ExpFAcosh         |             |
ExpIsNan          |             |
ExpIsInf          |             |
ExpRoundF         |             |
ExpCeilF          |             |
ExpFloorF         |             |
ExpToFloat        |             |
ExpFromFloat      |             |
ExpBitAnd         |             |
ExpBitOr          |             |
ExpBitXor         |             |
ExpBitComplement  |             |
ExpBitShiftL      |             |
ExpBitShiftR      |             |


