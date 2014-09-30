# Ivory Assertion Verification and Compilation to ACL2

[Ivory](https://github.com/GaloisInc/ivory) is a C like DSL embedded in [Haskell](http://haskell.org)
for hard realtime embedded applications.  [Galois](http://corp.galois.com/) is currently
using Ivory to build a [quadcopter autopilot](http://smaccmpilot.org/) for [DARPA](http://www.darpa.mil/)'s
[HACMS](http://www.darpa.mil/Our_Work/I2O/Programs/High-Assurance_Cyber_Military_Systems_(HACMS).aspx) program.

This library provides a means to verify Ivory assertions, and hence optimize them out of the generated code,
and to compile complete Ivory programs into ACL2 for higher levels of formal verification.

# Installation

1. Install the [Haskell Platform](http://www.haskell.org/platform/).
1. Install the Ivory DSL:
   1. Clone the [ivory](https://github.com/GaloisInc/ivory) git repo.
   1. Build and install the ivory sub library:

      `$ cd <ivory-repo-directory>/ivory && cabal install`

1. Install Ivory's ACL2 backend:
   1. Clone the [ivory-backend-acl2](https://github.com/tomahawkins/ivory-backend-acl2) repo.
   1. Build and install the ivory-backend-acl2 library:

      `$ cd <ivory-backend-acl2-directory>/ivory-backend-acl2 && cabal install`

1. Install [ACL2](http://www.cs.utexas.edu/users/moore/acl2/) and set the __ACL2_SOURCES__
   environment variable to point to the installation location:

   `$ export ACL2_SOURCES=<path-to-acl2-sources>`

# Verifying and Optimizing Out Ivory Assertions

The library provides the function `Ivory.Opts.Asserts.assertsFold` to check
and remove verified assertions in an Ivory program:

```haskell
assertsFold :: [Module] -> IO [Module]
```

`assertsFold` traverses the entire program, analyzing each functions' assertions and input and output contracts
(`requires` and `ensures`).
Interprocedural analysis is handled by using callees' IO contracts to abstract the function's body.
Each assertion is translated into an intermediate
[verification conditions language](https://github.com/tomahawkins/ivory-backend-acl2/blob/master/src/Ivory/Opts/Asserts/VC.hs)
(VC), which is then translated to ACL2 and checked.
Assertions that prove correct are rewritten as Ivory comments to annotate
the generated code.  Assertions that fail remain in place as runtime checks.
During the analysis, prior assertions in a program serve as lemmas for later ones.

## Possible Future Extensions

* Modeling a non-empty stack as a starting condition.
  * A free initial stack sometimes gave ACL2 trouble, e.g. structTest.
* Analyzing functions for purity and maintaining the stack across pure function calls.
* Loop analysis.
* Use the stack to handle global memory areas.
* Checking all call-sites to optimize out input contracts (requires).
* Targeting other provers (SMT).


# Compiling Ivory into ACL2

In addition to verifying assertions, this library provides means to translate Ivory programs to ACL2,
enabling higher level properties (written in ACL2) to be verified against Ivory implementations.

## An Example

In this example we will write a factorial function in Ivory, compile it to ACL2,
and then prove termination and other properties of the function.

We are going to create a file (Factorial.hs) to capture the program and run the verification.
First we need some pragmas to allow certain Haskell extensions leveraged by the Ivory language.
We also need to import the Ivory DSL, the Ivory-to-ACL2 compiler, and the ACL2 DSL:

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Ivory.Language
import Ivory.Compile.ACL2
import qualified Mira.ACL2 as A
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

Now, package the factorial function into a module:
```haskell
factorialModule :: Module
factorialModule = package "factorial" $ incl factorial
```

Next, compile the factorial module to ACL2:
```haskell
factorialACL2 :: [A.Expr]
factorialACL2 = compile factorialModule
```

Now that we have the Ivory factorial function in ACL2,
we just need to run it through ACL2 to check termination.
Since ACL2 requires termination of all defined functions,
if ```A.check``` returns true, this means ACL2 did not produce any errors
and the function was proven to terminate:

```haskell
main :: IO ()
main = do
  terminates <- A.check factorialACL2
  putStrLn $ "Termination: " ++ (if terminates then "pass" else "fail")
```

Finally, run the program to prove (or disprove) termination of the Ivory factorial function:

```
$ runhaskell -W Factorial.hs
```

This should result in `Termination: pass` printed on stdout.

Next, change the function so it doesn't terminate.  This can be accomplished by
negating the condition in the if-then-else branch:

```
factorial = proc "factorial" $ \ n -> body $
  ifte_ (iNot $ n >? 1)
    ...
```

This time the verification results in `Termination: fail`.

## Proving Theorems via Haskell Using the ACL2 DSL

In addition to termination, we can prove arbitrary
properties of the factorial function.  We do this by
extending the ACL2 expression list generated by the compiler.
For example, here is a theorem that proves `factorial 4 == 25`:

```haskell
main = do
   ...
   test <- A.check $ factorialACL2 ++ [A.thm $ A.equal 24 $ A.cdr $ A.call "factorial" [A.nil, 4]]
   putStrLn $ "factorial 4 == 24: " ++ (if test then "pass" else "fail")
```

Rerunning the program should yield `factorial 4 == 24: pass`.


## A Closer Look at the Compilation Process

Running the above example produces the following files,
which are the various intermediate representations (IRs)
of the Ivory to ACL2 compiler flow:

1. CLL

  The first step in the translation converts the
  [Ivory AST](https://github.com/GaloisInc/ivory/blob/master/ivory/src/Ivory/Language/Syntax/AST.hs)
  to a smaller, simpler form called
  [CLL](https://github.com/tomahawkins/ivory-backend-acl2/blob/master/src/Ivory/Compile/ACL2/CLL.hs).
  This smaller language provides top level function definitions
  and Call, If, Return, Assert, Let, and Loop statements
  along with a host of [Expressions](https://github.com/tomahawkins/ivory-backend-acl2/blob/master/mira/src/Ivory/Compile/ACL2/Expr.hs).

1. CPS

  From CLL we then translate into a continuation passing style
  ([CPS](https://github.com/tomahawkins/ivory-backend-acl2/blob/master/src/Ivory/Compile/ACL2/CPS.hs))
  form.  In CPS expression evaluation order is made explicit
  and all function calls become tail-calls.
  This CPS IR provides the following continuation types:  Halt, Call, Return, Let, If, and Assert.
  In addition, this IR also provides for explicit stack operations (Push, Pop).


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
Assign         | Variable assignment.                                | X           |          
RefCopy        | Copy a reference.                                   | X           |          
Forever        | A forever loop.                                     |             |          
Break          | A break statement for a loop.                       |             |          

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


