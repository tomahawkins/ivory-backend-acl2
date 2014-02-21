{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Ivory.Language
import Ivory.Compile.ACL2
import Ivory.Compile.C.CmdlineFrontend

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
  --a <- call factorial 3
  --assert $ a ==? 6
  assert true
  retVoid

module' :: Module
module' = package "Factorial" $ do
  incl factorial
  incl main'

main :: IO ()
main = do
  compileModule module'
  runCompiler [module'] initialOpts


