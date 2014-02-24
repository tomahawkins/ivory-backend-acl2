-- | Compiling Ivory to ACL2.
module Ivory.Compile.ACL2
  ( compileModule
  , verifyModule
  , verifyModules
  ) where

import Data.List
import System.IO
import System.Process

import Ivory.Compile.ACL2.ACL2 (Expr)
import Ivory.Compile.ACL2.ACL2Convert
import Ivory.Compile.ACL2.CPS (alphaConvert, Proc)
import Ivory.Compile.ACL2.CPSConvert
import Ivory.Compile.ACL2.RTL (Program)
import Ivory.Compile.ACL2.RTLConvert
import qualified Ivory.Language.Syntax.AST as I
import Ivory.Language.Syntax.AST (Module (..), ExpOp)

compileModule :: Module -> (String, [Proc ExpOp], [Proc ExpOp], Program ExpOp, [Expr])
compileModule m = (name, cps1, cps2, rtl, acl2)
  where
  cps1 = map cpsConvertProc $ procs m 
  cps2 = alphaConvert cps1
  rtl = rtlConvert cps2
  acl2 = acl2Convert rtl
  name = modName m
  procs :: I.Module -> [I.Proc]
  procs m = I.public (I.modProcs m) ++ I.private (I.modProcs m)
  --writeFile (name ++ ".cps1") $ unlines $ map show cps1
  --writeFile (name ++ ".cps2") $ unlines $ map show cps2
  --writeFile (name ++ ".rtl")  $ show rtl
  --writeFile (name ++ ".lisp") $ unlines $ map show acl2

verifyModule :: Module -> Bool -> IO Bool
verifyModule m expectedPass = do
  putStr $ "Verifying " ++ name ++ " ... "
  hFlush stdout
  (_, result, _) <- readProcessWithExitCode "acl2" [] (unlines $ map show acl2)
  let pass = expectedPass == (not $ any (isPrefixOf "ACL2 Error") $ lines result)
  putStrLn $ if pass then "pass" else "FAIL"
  writeFile (name ++ ".log") result
  hFlush stdout
  return pass
  where
  (name, _, _, _, acl2) = compileModule m

verifyModules :: [(Module, Bool)] -> IO Bool
verifyModules m = do
  pass <- sequence [ verifyModule a b | (a, b) <- m ]
  return $ and pass

