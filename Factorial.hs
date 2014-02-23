{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

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

main' :: Def ('[] :-> ())
main' = proc "main" $ body $ do
  --Q: All these assertions pass individually.  But if more are uncommented, runtime goes through the roof.  Why?
  --a <- call factorial 3
  --assert $ a ==? 6
  --assert true
  --assert $ 1 + 2 ==? (3 :: Sint32)
  --assert $ 3 - 2 ==? (1 :: Sint32)
  --assert $ (false .&& false) ==? false
  --assert $ (false .&& true ) ==? false
  --assert $ (true  .&& false) ==? false
  --assert $ (true  .&& true ) ==? true 
  --assert $ (false .|| false) ==? false
  --assert $ (false .|| true ) ==? true
  --assert $ (true  .|| false) ==? true
  --assert $ (true  .|| true ) ==? true 
  --assert $ true  ? (true , false)
  --assert $ true  ? (true , true )
  --assert $ false ? (false, true )
  --assert $ false ? (true , true )

  -- This works.
  a <- call factorial 1
  assert $ a ==? 1

  -- This fails.  Why?
  --a <- call factorial 2
  --assert $ a ==? 2

  retVoid

simple :: String -> Bool -> (forall s . Ivory (ProcEffects s ()) ()) -> (Module, Bool)
simple name expected a = (package name $ incl $ proc "main" $ body $ a >> retVoid, expected)

module' :: Module
module' = package "factorial" $ do
  incl factorial
  incl main'

main :: IO ()
main = do
  result <- verifyModules
    [ simple "01" True  $ assert true
    , simple "01" False $ assert false
    , simple "02" True  $ assert $ 1 + 2 ==? (3 :: Sint32)
    , simple "02" False $ assert $ 1 + 2 ==? (4 :: Sint32)
    , simple "03" True  $ assert $ 3 - 2 ==? (1 :: Sint32)
    , simple "03" False $ assert $ 2 - 2 ==? (1 :: Sint32)
    , simple "04" True  $ assert $ (false .&& false) ==? false
    , simple "05" True  $ assert $ (false .&& true ) ==? false
    , simple "06" True  $ assert $ (true  .&& false) ==? false
    , simple "07" True  $ assert $ (true  .&& true ) ==? true 
    , simple "08" True  $ assert $ (false .|| false) ==? false
    , simple "09" True  $ assert $ (false .|| true ) ==? true
    , simple "10" True  $ assert $ (true  .|| false) ==? true
    , simple "11" True  $ assert $ (true  .|| true ) ==? true 
    , simple "12" True  $ assert $ true  ? (true , false)
    , simple "13" True  $ assert $ true  ? (true , true )
    , simple "14" True  $ assert $ false ? (false, true )
    , simple "15" True  $ assert $ false ? (true , true )
    , (module', True)
    ]
  if result
    then putStrLn "Tests passed."
    else putStrLn "Tests failed."


