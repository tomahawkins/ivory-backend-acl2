-- Testcases for Ivory to ACL2 compilation.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import System.Environment

import Ivory.Language
import Ivory.Compile.ACL2

type Stmt = forall s . Ivory (ProcEffects s ()) ()

intrinsicTest :: Def ('[] :-> ())
intrinsicTest = proc "intrinsicTest" $ body $ do
  assert $ true
  assert $ iNot false
  assert $ 1 + 2 ==? (3 :: Sint32)
  assert $ iNot $ 1 + 2 ==? (4 :: Sint32)
  assert $ 3 - 2 ==? (1 :: Sint32)
  assert $ iNot $ 2 - 2 ==? (1 :: Sint32)
  assert $ 1 /=? (2 :: Sint32)
  assert $ iNot $ 1 /=? (1 :: Sint32)
  assert $ 1 <?  (2 :: Sint32)
  assert $ 3 >?  (2 :: Sint32)
  assert $ 1 <=? (1 :: Sint32)
  assert $ 3 >=? (3 :: Sint32)
  assert $ iNot false
  assert $ iNot $ iNot true
  assert $ iNot $ iNot $ iNot false
  assert $ iNot $ iNot $ iNot $ iNot true
  assert $ (false .&& false) ==? false
  assert $ (false .&& true ) ==? false
  assert $ (true  .&& false) ==? false
  assert $ (true  .&& true ) ==? true 
  assert $ (false .|| false) ==? false
  assert $ (false .|| true ) ==? true
  assert $ (true  .|| false) ==? true
  assert $ (true  .|| true ) ==? true 
  assert $ true  ? (true , false)
  assert $ true  ? (true , true )
  assert $ false ? (false, true )
  assert $ false ? (true , true )
  assert $ (3 .% 7) ==? (3 :: Sint32)
  assert $ negate 3 ==? (-3 :: Sint32)
  assert $ abs (-3) ==? (3 :: Sint32)
  assert $ signum 0 ==? (0 :: Sint32)
  -- A test of assumptions.
  assume false
  assert false
  retVoid

-- Factorial of a number.
factorial :: Def ('[Sint32] :-> Sint32)
factorial  = proc "factorial" $ \ n -> body $
  ifte_ (n >? 1)
   (do n' <- call factorial (n - 1)
       ret (n' * n)
   )
   (do ret n)
  
-- Tests of calling the factorial function.
factorialTests :: [(Bool, Module)]
factorialTests =
  [ factorialTest "factorial1" True $ do
      a <- call factorial 1
      assert $ a ==? 1
  , factorialTest "factorial2" True $ do
      a <- call factorial 2
      assert $ a ==? 2
  ]
  where
  factorialTest :: String -> Bool -> Stmt -> (Bool, Module)
  factorialTest name expected a = (expected, m)
    where
    m = package name $ do
      incl factorial
      incl $ proc "main" $ body $ do
        a
        retVoid

-- A test of loops and arrays.
loopTest :: Def ('[Ix 10] :-> Uint32)
loopTest = proc "loopTest" $ \ix ->
     ensures (<=? 10)
   $ body
   $ do
   ref <- local (ival 0)
   ix `times` \_ -> do
     n <- deref ref
     store ref (n+1)
   ret =<< deref ref

-- A termination test of infinite recursion.
infiniteRecursionTest :: Def ('[] :-> Uint32)
infiniteRecursionTest = proc "callForever" $ body $ do
  call infiniteRecursionTest
  ret 0

--struct Foo { i    :: Stored Uint32 }
--[ivory|
--struct Bar { name :: Array 32 (Stored Uint32) }
-- |]
[ivory|
struct Foo { i :: Stored Uint32 }
struct Bar { name :: Array 32 (Stored Uint32) }
|]

structTest :: Def ('[] :-> Uint32)
structTest = proc "structTest" $ body $ do
  struct <- local $ istruct [ i .= ival 22 ]
  a <- deref $ struct ~> i
  ret a

arrayTest :: Def ('[] :-> Uint32)
arrayTest = proc "arrayTest" $ {- ensures (.= 6) $ -} body $ do
  -- Allocate a 4 element array with zeros: [0, 0, 0, 0]
  (array :: Ref (Stack cs) (Array 4 (Stored Uint32))) <- local $ iarray $ replicate 4 $ ival 1

  -- Iterate over the array making it: [0, 1, 2, 3]
  arrayMap $ \ i -> store (array ! i) $ safeCast i  --XXX Having both this loop and the loop below gives ACL2 troubles.

  -- Create a reference to sum the elements in the array.
  sum <- local $ ival (0 :: Uint32)

  -- Loop across the array summing the elements.
  arrayMap $ \ i ->  do
    n <- deref sum
    m <- deref $ array ! i
    store sum $ n + m

  assert true

  -- Return the computed sum.
  deref sum >>= ret


main :: IO ()
main = do
  test "termination: intrinsicTest" $ verifyTermination $ package "intrinsicTest" $ incl intrinsicTest
  test "termination: factorial"     $ verifyTermination $ package "factorial"     $ incl factorial
  test "termination: loopTest"      $ verifyTermination $ package "loopTest"      $ incl loopTest
  test "termination: arrayTest"     $ verifyTermination $ package "arrayTest"     $ incl arrayTest
  --test "assertions: intrinsicTest"  $ verifyAssertions  $ package "intrinsicTest" $ incl intrinsicTest
  --test "assertions: arrayTest"      $ verifyAssertions  $ package "arrayTest"     $ incl arrayTest

test :: String -> IO Bool -> IO ()
test name action = do
  pass <- action
  if pass
    then putStrLn $ "pass: " ++ name
    else putStrLn $ "FAIL: " ++ name

