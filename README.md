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

1. Install [ACL2](http://www.cs.utexas.edu/users/moore/acl2/) and create a symbolic link
   named __acl2__ to the ACL2 executable, usually called __saved\_acl2__.  Ensure the link is in the search path.

   `$ ln -s <path-to-acl2>/saved_acl2 acl2`

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

## A Closer Look at the Compilation Process

Running the above example produces the following files:

- factorial.cll
- factorial.cps1
- factorial.cps2
- factorial.rtl
- factorial-cps.lisp
- factorial-rtl.lisp

TODO


# Known Bugs and Limitations

- Unsupported Ivory features:
  - Mutable state.
  - Structures.
  - Pointer operations.
  - Post condition checks.

