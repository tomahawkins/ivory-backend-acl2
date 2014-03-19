{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ImpredicativeTypes #-}
module Main (main) where

import Ivory.Language
import Ivory.Compile.ACL2

someLoopFunc :: Def ('[Ix 10] :-> Uint32)
someLoopFunc = proc "someLoopFunc" $ \ix ->
     ensures (<=? 10)
   $ body
   $ do
   {-
   ref <- local (ival 0)
   ix `times` \_ -> do
     n <- deref ref
     store ref (n+1)
   ret =<< deref ref
   -}
   call someLoopFunc ix
   ret 0

main :: IO ()
main = do
  pass <- verifyTermination $ package "loop" $ incl someLoopFunc
  putStrLn (if pass then "pass" else "FAIL")

