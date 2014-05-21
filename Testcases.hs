-- Testcases for Ivory to ACL2 compilation.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

arrayTest :: Def ('[] :-> Uint32)
arrayTest = proc "arrayTest" $ body $ do
  -- Allocate a 4 element array with elements [0, 1, 2, 3].
  (array :: Ref (Stack cs) (Array 4 (Stored Uint32))) <- local $ iarray $ map ival [0, 1, 2, 3]

  -- Create a reference to sum the elements in the array.
  sum <- local $ ival (0 :: Uint32)

  -- Loop across the array summing the elements.
  arrayMap $ \ i ->  do
     n <- deref sum
     m <- deref $ array ! i
     store sum $ n + m

  -- Return the computed sum.
  deref sum >>= ret


-- A list of all testcases.
allTests :: [(Bool, Module)]
allTests = concat
  [ basicTests
  , factorialTests
  ]

main :: IO ()
main = do
  putStrLn "Basic tests:"
  --result <- verifyModules allTests
  --if result
  --  then putStrLn "Tests passed."
  --  else putStrLn "Tests failed."

  putStrLn "Termination tests:"
  --verifyTermination' "factorial" $ incl factorial
  --verifyTermination' "loopTest" $ incl loopTest
  --verifyTermination' "infiniteRecursionTest" $ incl infiniteRecursionTest
  verifyTermination' "arrayTest" $ do incl arrayTest
  --pass <- verifyTermination' "structArrayTest" $ do
  --  defStruct (Proxy :: Proxy "Foo")
  --  defStruct (Proxy :: Proxy "Bar")
  --  incl structArrayTest
  where
  verifyTermination' name a =  do
    pass <- verifyTermination $ package name $ a
    putStrLn $ name ++ " terminates: " ++ (if pass then "pass" else "FAIL")


