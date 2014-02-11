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

fac :: Module
fac = package "Factorial" $ incl factorial

main :: IO ()
main = do
  compileModule fac
  runCompiler [fac] initialOpts


