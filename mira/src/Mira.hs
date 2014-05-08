module Mira
  ( compile
  ) where

import Mira.CLL hiding (Expr)
import Mira.CPS (explicitStack)
import Mira.ACL2ConvertCPS
import Mira.ACL2ConvertRTL
import Mira.CPSConvert
import Mira.RTLConvert

-- | Compile CLL to several different forms.
compile :: String -> [Proc] -> IO ()
compile name cll = do
  writeFile (name ++ ".cll")  $ unlines $ map show cll 
  writeFile (name ++ ".cps1") $ unlines $ map show cps1
  writeFile (name ++ ".cps2") $ unlines $ map show cps2
  writeFile (name ++ "-cps.lisp") $ unlines $ map show acl2CPS
  writeFile (name ++ ".rtl")  $ show rtl 
  writeFile (name ++ "-rtl.lisp") $ unlines $ map show acl2RTL
  where
  cps1    = cpsConvert        cll 
  cps2    = map explicitStack cps1
  rtl     = rtlConvert        cps2
  acl2CPS = acl2ConvertCPS    cps2
  acl2RTL = acl2ConvertRTL    rtl 

