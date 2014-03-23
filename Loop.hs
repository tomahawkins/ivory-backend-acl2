{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Ivory.Language
import Ivory.Compile.ACL2

someLoopFunc :: Def ('[Ix 10] :-> Uint32)
someLoopFunc = proc "someLoopFunc" $ \ix ->
     ensures (<=? 10)
   $ body
   $ do
   ref <- local (ival 0)
   ix `times` \_ -> do
     n <- deref ref
     store ref (n+1)
   ret =<< deref ref

callForever :: Def ('[] :-> Uint32)
callForever = proc "callForever" $ body $ do
  call callForever
  ret 0

main :: IO ()
main = do
  pass <- verifyTermination $ package "loop" $ incl someLoopFunc
  putStrLn (if pass then "pass" else "FAIL")
  pass <- verifyTermination $ package "callForever" $ incl callForever
  putStrLn (if not pass then "pass" else "FAIL")

