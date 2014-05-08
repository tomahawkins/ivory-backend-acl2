-- Testcases for Ivory to ACL2 compilation.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Ivory.Language
import Ivory.Compile.ACL2

type Stmt = forall s . Ivory (ProcEffects s ()) ()

-- A collection of some basic tests.
basicTests :: [(Bool, Module)]
basicTests =
  [ basicTest "basic00"   True  $ assert true
  , basicTest "basic01"   False $ assert false
  , basicTest "basic02"   True  $ assert $ 1 + 2 ==? (3 :: Sint32)
  , basicTest "basic03"   False $ assert $ 1 + 2 ==? (4 :: Sint32)
  , basicTest "basic04"   True  $ assert $ 3 - 2 ==? (1 :: Sint32)
  , basicTest "basic05"   False $ assert $ 2 - 2 ==? (1 :: Sint32)
  , basicTest "basic0501" True  $ assert $ 1 /=? (2 :: Sint32)
  , basicTest "basic0502" False $ assert $ 1 /=? (1 :: Sint32)
  , basicTest "basic0503" True  $ assert $ 1 <?  (2 :: Sint32)
  , basicTest "basic0504" True  $ assert $ 3 >?  (2 :: Sint32)
  , basicTest "basic0505" True  $ assert $ 1 <=? (1 :: Sint32)
  , basicTest "basic0506" True  $ assert $ 3 >=? (3 :: Sint32)
  , basicTest "basic06"   True  $ assert $ iNot false
  , basicTest "basic07"   True  $ assert $ iNot $ iNot true
  , basicTest "basic08"   False $ assert $ iNot $ iNot false
  , basicTest "basic09"   False $ assert $ iNot $ iNot $ iNot true
  , basicTest "basic10"   True  $ assert $ (false .&& false) ==? false
  , basicTest "basic11"   True  $ assert $ (false .&& true ) ==? false
  , basicTest "basic12"   True  $ assert $ (true  .&& false) ==? false
  , basicTest "basic13"   True  $ assert $ (true  .&& true ) ==? true 
  , basicTest "basic14"   True  $ assert $ (false .|| false) ==? false
  , basicTest "basic15"   True  $ assert $ (false .|| true ) ==? true
  , basicTest "basic16"   True  $ assert $ (true  .|| false) ==? true
  , basicTest "basic17"   True  $ assert $ (true  .|| true ) ==? true 
  , basicTest "basic18"   True  $ assert $ true  ? (true , false)
  , basicTest "basic19"   True  $ assert $ true  ? (true , true )
  , basicTest "basic20"   True  $ assert $ false ? (false, true )
  , basicTest "basic21"   True  $ assert $ false ? (true , true )
  , basicTest "basic22"   True  $ assert $ (3 .% 7) ==? (3 :: Sint32)
  , basicTest "basic23"   True  $ assert $ negate 3 ==? (-3 :: Sint32)
  , basicTest "basic24"   True  $ assert $ abs (-3) ==? (3 :: Sint32)
  , basicTest "basic25"   True  $ assert $ signum 0 ==? (0 :: Sint32)
  ]
  where
  basicTest :: String -> Bool -> Stmt -> (Bool, Module)
  basicTest name expected a = (expected, package name $ incl $ proc "main" $ body $ a >> retVoid)


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

-- A test of structures, arrays, and pointers.
structArrayTest :: Def ('[Ref s (Struct "Bar")] :-> Uint32)
structArrayTest = proc "structArrayTest" $ \ s -> body $ do
  a <- deref $ (s ~> name) ! 0
  ret a


{-

-- Limits a number between two bounds.
limit :: Def ('[Sint32, Sint32, Sint32] :-> Sint32)
limit = proc "limit" $ \ low high n ->
  requires (low <=? high) $
  ensures  (<=? high)     $
  ensures  (>=? low)      $
  body $ ifte_ (n >? high) (ret high) $ ifte_ (n <? low) (ret low) (ret n)


-- Build a test using the limit function.
limitTest :: String -> Bool -> Stmt -> (Bool, Module)
limitTest name expected a = (expected, m)
  where
  m = package name $ do
    incl limit
    incl $ proc "main" $ body $ do
      a
      retVoid

-- Combine all the positive tests above into one test.  (ACL2 can't handle the increased size.)
--combinedBasicTest :: (Bool, Module)
--combinedBasicTest = basicTest "combinedBasicTest" True $ sequence_ [ b | (_, a, b) <- basicTests, a ]

-- Some tests calling the limit function.
limitTests :: [(Bool, Module)]
limitTests =
  [ limitTest "limit1" True $ do
      a <- call limit 32 48 56
      assert $ a ==? 48
  , limitTest "limit2" True $ do
      a <- call limit 32 48 20
      assert $ a ==? 32
  , limitTest "limit3" True $ do
      a <- call limit 32 48 42
      assert $ a ==? 42
  , limitTest "limit4" False $ do
      _ <- call limit 48 32 40  -- Check that the precondition fails.
      return ()
  ]
-}


-- A list of all testcases.
allTests :: [(Bool, Module)]
allTests = concat
  [ basicTests
  , factorialTests
  ]

main :: IO ()
main = do
  --result <- verifyModules allTests
  --if result
  --  then putStrLn "Tests passed."
  --  else putStrLn "Tests failed."
  --putStrLn "Termination tests:"
  pass <- verifyTermination $ package "loopTest" $ incl loopTest
  putStrLn (if pass then "pass" else "FAIL")
  --pass <- verifyTermination $ package "infiniteRecursionTest" $ incl infiniteRecursionTest
  --putStrLn (if not pass then "pass" else "FAIL")
  --pass <- verifyTermination $ package "structArrayTest" $ do
  --  defStruct (Proxy :: Proxy "Foo")
  --  defStruct (Proxy :: Proxy "Bar")
  --  incl structArrayTest
  --putStrLn (if pass then "pass" else "FAIL")

