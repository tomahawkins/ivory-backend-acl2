-- Code from the README.md example.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Ivory.Language
import Ivory.Compile.ACL2
import qualified Mira.ACL2 as A

-- Factorial of a number.
factorial :: Def ('[Sint32] :-> Sint32)
factorial  = proc "factorial" $ \ n -> body $
  ifte_ (n >? 1)
   (do n' <- call factorial (n - 1)
       ret (n' * n)
   )
   (do ret n)

-- Package the factorial function into a module.
factorialModule :: Module
factorialModule = package "factorial" $ incl factorial

-- Compile the factorial module to ACL2.
factorialACL2 :: [A.Expr]
factorialACL2 = compile factorialModule

main :: IO ()
main = do
  --mapM_ print factorialACL2

  terminates <- A.check factorialACL2
  putStrLn $ "Termination: " ++ (if terminates then "pass" else "fail")

  test <- A.check $ factorialACL2 ++ [A.thm $ A.equal 24 $ A.cdr $ A.call "factorial" [A.nil, 4]]
  putStrLn $ "factorial 4 == 24: " ++ (if test then "pass" else "fail")

  --`where

  {-
  let a = A.var "a"
      fac a = A.cdr $ A.call "factorial" [A.nil, a]
      t = A.implies (a A.>. 1) (fac a A.>. a)
  test <- A.check' $ compile factorialModule ++ [A.thm t]
  putStrLn $ "a > 0 -> factorial a >= a: " ++ (if test then "pass" else "fail")
  -}



  {-
  test "assertions: intrinsicTest"  $ verifyAssertions  $ package "intrinsicTest" $ incl intrinsicTest
  --test "assertions: arrayTest"      $ verifyAssertions  $ package "arrayTest"     $ incl arrayTest
  testThm "factorial 4 == 24" factorial  $ A.equal 24 $ A.cdr $ A.call "factorial"  [A.nil, 4]
  testThm "arrayTest   ==  6" arrayTest  $ A.equal  6 $ A.cdr $ A.call "arrayTest"  [A.nil]
  testThm "loopTest  8 ==  8" loopTest   $ A.equal  8 $ A.cdr $ A.call "loopTest"   [A.nil, 8]
  testThm "structTest  == 22" structTest $ A.equal 22 $ A.cdr $ A.call "structTest" [A.nil]
  where
  testThm name func thm = test name $ A.check $ compile (package name $ incl func) ++ [A.thm thm]

  test :: String -> IO Bool -> IO ()
  test name action = do
    pass <- action
    if pass
      then putStrLn $ "pass: " ++ name
      else putStrLn $ "FAIL: " ++ name
      -}

