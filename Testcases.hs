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
import Ivory.Opts.Asserts
import qualified Mira.ACL2 as A

--type Stmt = forall s . Ivory (ProcEffects s ()) ()

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

--struct Foo { i    :: Stored Uint32 }
--[ivory|
--struct Bar { name :: Array 32 (Stored Uint32) }
-- |]
[ivory|
struct Foo { i :: Stored Uint32 }
--struct Bar { name :: Array 32 (Stored Uint32) }
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

verifyAssertions :: Module -> IO ()
verifyAssertions m = assertsFold [m] >> return ()

main :: IO ()
main = do
  --mapM_ print $ compile (package "arrayTest" $ incl arrayTest)

  verifyAssertions $ package "intrinsicTest" $ incl intrinsicTest
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
