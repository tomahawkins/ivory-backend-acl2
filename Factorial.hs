{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
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

add :: Def ('[Sint32, Sint32] :-> Sint32)
add = proc "add" $ \ a b -> body $ ret $ a + b 

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
  --a <- call add 1 2
  --assert $ a ==? 3

  -- This works.
  a <- call factorial 1
  assert $ a ==? 1

  -- This fails.  Why?
  --a <- call factorial 2
  --assert $ a ==? 2

  retVoid


module' :: Module
module' = package "factorial" $ do
  incl add
  incl factorial
  incl main'

main :: IO ()
main = compileModule module'

