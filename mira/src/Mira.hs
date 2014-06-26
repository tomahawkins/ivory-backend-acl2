module Mira
  ( verifyTermination
  , verifyAssertions
  , compile
  ) where

--import System.Environment

import Mira.CLL hiding (Expr)
import Mira.CPS hiding (Proc)
import Mira.CPSConvert
import Mira.Verify
import Mira.ACL2 (check, Expr)
import Mira.ACL2ConvertCPS

-- | Verifies termination of a module.
verifyTermination :: [Proc] -> IO Bool
verifyTermination cll = check $ compile cll

-- | Compile an CLL procedures to ACL2.
compile :: [Proc] -> [Expr]
compile cll = acl2ConvertCPS $ map (removeNullEffect . removeAsserts . commonSubExprElim) $ cpsConvert cll

-- | Verifies assertions and pre/post conditions of procedures in a module.
verifyAssertions :: [Proc] -> IO Bool
verifyAssertions = verifyProcs

{-
acl2Sources :: IO FilePath
acl2Sources = do
  env <- getEnvironment
  case lookup "ACL2_SOURCES" env of
    Nothing -> error "Environment variables ACL2_SOURCES not found."
    Just a  -> return a
-}

{-
import System.Environment

import Mira.CLL hiding (Expr)
import Mira.CPS (removeNullEffect, removeAsserts, commonSubExprElim, explicitStack)
import Mira.ACL2ConvertCPS
import Mira.ACL2ConvertRTL
import Mira.CPSConvert
import Mira.RTLConvert

-- | Compile CLL to several different forms.
compile :: String -> [Proc] -> IO ()
compile name cll = do
  env <- getEnvironment 
  acl2Sources <- case lookup "ACL2_SOURCES" env of
    Nothing -> error $ "Environment variable ACL2_SOURCES not set."
    Just a  -> return a
  writeFile (name ++ ".cll")  $ unlines $ map show cll 
  writeFile (name ++ ".cps1") $ unlines $ map show cps1
  writeFile (name ++ ".cps2") $ unlines $ map show cps2
  writeFile (name ++ "-cps.lisp") $ unlines $ map show $ acl2CPS acl2Sources
  writeFile (name ++ ".rtl")  $ show rtl 
  writeFile (name ++ "-rtl.lisp") $ unlines $ map show acl2RTL
  where
  cps1    = map removeNullEffect $ map removeAsserts $ map commonSubExprElim $ cpsConvert cll 
  cps2    = map explicitStack cps1
  rtl     = rtlConvert        cps2
  acl2CPS a = acl2ConvertCPS a    cps2
  acl2RTL = acl2ConvertRTL    rtl 
-}

