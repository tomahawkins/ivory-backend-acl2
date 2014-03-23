module Mira
  ( compile
  ) where

import Mira.ACL2 (Expr)
import Mira.CLL hiding (Expr)
import Mira.CPS (Intrinsics, explicitStack)
import Mira.ACL2ConvertCPS
import Mira.ACL2ConvertRTL
import Mira.CPSConvert
import Mira.RTLConvert

-- | Compile CLL to several different forms.
compile :: (Show i, Intrinsics i) => String -> (i -> [Expr] -> Expr) -> (i -> Expr) -> (Expr -> (Int -> Expr) -> Expr) -> [Proc i] -> IO ()
compile name cpsIntrinsics rtlIntrinsicsCode rtlIntrinsicsImpl cll = do
  writeFile (name ++ ".cll")  $ unlines $ map show cll 
  writeFile (name ++ ".cps1") $ unlines $ map show cps1
  writeFile (name ++ ".cps2") $ unlines $ map show cps2
  writeFile (name ++ ".rtl")  $ show rtl 
  writeFile (name ++ "-cps.lisp") $ unlines $ map show acl2CPS
  writeFile (name ++ "-rtl.lisp") $ unlines $ map show acl2RTL
  where
  cps1  = cpsConvert cll 
  cps2  = map explicitStack cps1
  rtl   = rtlConvert        cps2
  acl2CPS = acl2ConvertCPS cpsIntrinsics cps2
  acl2RTL = acl2ConvertRTL rtlIntrinsicsCode rtlIntrinsicsImpl rtl 

