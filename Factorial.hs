{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main (main) where

import Ivory.Language
import Ivory.Compile.ACL2

factorial :: Def ('[Sint32] :-> Sint32)
factorial  = proc "factorial" $ \ n ->
  -- These are made up requires/ensures for testing purposes.
  ensures (\r -> n <? r) $
  body $
    ifte_ (n >? 1)
      (do n' <- call factorial (n - 1)
          ret (n' * n)
      )
      (do ret n
      )

type Stmt = forall s . Ivory (ProcEffects s ()) ()

-- Build a basic test given a name, whether the test should pass or fail, and an Ivory statement.
basicTest :: String -> Bool -> Stmt -> (Module, Bool)
basicTest name expected a = (package name $ incl $ proc "main" $ body $ a >> retVoid, expected)

-- Build a test that uses the above factorial function.
factorialTest :: String -> Bool -> Stmt -> (Module, Bool)
factorialTest name expected a = (m, expected)
  where
  m = package name $ do
    incl factorial
    incl $ proc "main" $ body $ do
      a
      retVoid

-- A collection of some basic tests.
basicTests :: [(String, Bool, Stmt)]
basicTests =
  [ f "basic00" True  $ assert true
  , f "basic01" False $ assert false
  , f "basic02" True  $ assert $ 1 + 2 ==? (3 :: Sint32)
  , f "basic03" False $ assert $ 1 + 2 ==? (4 :: Sint32)
  , f "basic04" True  $ assert $ 3 - 2 ==? (1 :: Sint32)
  , f "basic05" False $ assert $ 2 - 2 ==? (1 :: Sint32)
  , f "basic06" True  $ assert $ iNot false
  , f "basic07" True  $ assert $ iNot $ iNot true
  , f "basic08" False $ assert $ iNot $ iNot false
  , f "basic09" False $ assert $ iNot $ iNot $ iNot true
  , f "basic10" True  $ assert $ (false .&& false) ==? false
  , f "basic11" True  $ assert $ (false .&& true ) ==? false
  , f "basic12" True  $ assert $ (true  .&& false) ==? false
  , f "basic13" True  $ assert $ (true  .&& true ) ==? true 
  , f "basic14" True  $ assert $ (false .|| false) ==? false
  , f "basic15" True  $ assert $ (false .|| true ) ==? true
  , f "basic16" True  $ assert $ (true  .|| false) ==? true
  , f "basic17" True  $ assert $ (true  .|| true ) ==? true 
  , f "basic18" True  $ assert $ true  ? (true , false)
  , f "basic19" True  $ assert $ true  ? (true , true )
  , f "basic20" True  $ assert $ false ? (false, true )
  , f "basic21" True  $ assert $ false ? (true , true )
  ]
  where
  f :: String -> Bool -> Stmt -> (String, Bool, Stmt)
  f a b c = (a, b, c)

-- Combine all the positive tests above into one test.  (ACL2 can't handle the increased size.)
combinedBasicTest :: (Module, Bool)
combinedBasicTest = basicTest "combinedBasicTest" True $ sequence_ [ b | (_, a, b) <- basicTests, a ]

-- A couple tests of calling the factorial function.
factorialTests :: [(Module, Bool)]
factorialTests =
  [ factorialTest "factorial1" True $ do
      a <- call factorial 1
      assert $ a ==? 1
  , factorialTest "factorial2" True $ do
      a <- call factorial 2
      assert $ a ==? 2
  ]

main :: IO ()
main = do
  result <- verifyModules
    $  [ basicTest a b c | (a, b, c) <- basicTests ]
    ++ factorialTests
    -- ++ [combinedBasicTest]  -- This test is too large; ACL2 doesn't return.
  if result
    then putStrLn "Tests passed."
    else putStrLn "Tests failed."

