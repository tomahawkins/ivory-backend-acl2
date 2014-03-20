-- | Compiling Ivory to ACL2.
module Ivory.Compile.ACL2
  ( compileModule
  , verifyModule
  , verifyModules
  , verifyTermination
  ) where

import Data.List
import System.IO
import System.Process

import Mira.ACL2
import Mira.ACL2ConvertCPS
import Mira.ACL2ConvertRTL
import Mira.CPS (explicitStack)
import Mira.RTLConvert

import Ivory.Compile.ACL2.CPSConvert
import qualified Ivory.Language.Syntax.AST as I
import Ivory.Language.Syntax.AST (Module (..), ExpOp (..))

-- | Compiles a module to two different ACL2 representations: assembly and CPS.
compileModule :: Module -> (String, [Expr], [Expr])
compileModule m = (name, acl21, acl22)
  where
  cps1  = cpsConvert $ procs m 
  cps2  = map explicitStack cps1
  rtl   = rtlConvert        cps2
  acl21 = acl2ConvertRTL    rtl 
  acl22 = acl2ConvertCPS intrinsics cps2
  name = modName m
  procs :: I.Module -> [I.Proc]
  procs m = I.public (I.modProcs m) ++ I.private (I.modProcs m)

-- Intrinsic implementation for ACL2 (CPS form).
intrinsics :: ExpOp -> [Expr] -> Expr
intrinsics op args = case op of
  ExpEq   _        -> if' (equal (arg 0) (arg 1)) 1 0
  ExpNeq  _        -> if' (not' (equal (arg 0) (arg 1))) 1 0
  ExpCond          -> if' (zip' (arg 0)) (arg 2) (arg 1)
  ExpGt   False _  -> if' (call ">"  [arg 0, arg 1]) 1 0
  ExpGt   True  _  -> if' (call ">=" [arg 0, arg 1]) 1 0
  ExpLt   False _  -> if' (call "<"  [arg 0, arg 1]) 1 0
  ExpLt   True  _  -> if' (call "<=" [arg 0, arg 1]) 1 0
  ExpNot           -> if' (zip' (arg 0)) 1 0
  ExpAnd           -> if' (or'  (zip' (arg 0)) (zip' (arg 1))) 0 1
  ExpOr            -> if' (and' (zip' (arg 0)) (zip' (arg 1))) 0 1
  ExpMul           -> arg 0 * arg 1
  ExpAdd           -> arg 0 + arg 1
  ExpSub           -> arg 0 - arg 1
  ExpNegate        -> 0 - arg 1
  ExpAbs           -> if' (call ">=" [arg 0, 0]) (arg 0) (0 - arg 0)
  ExpSignum        -> if' (call ">"  [arg 0, 0]) 1 $ if' (call "<" [arg 0, 0]) (-1) 0
  a -> error $ "Unsupported intrinsic: " ++ show a
  where
  arg n = args !! n

-- | Given a expected result, verifies a module.
verifyModule :: Bool -> Module -> IO Bool
verifyModule expected m = do
  putStr $ "Verifying termination of: " ++ name ++ " ... "
  hFlush stdout
  terminates <- verifyTermination m
  putStrLn $ if terminates then "pass" else "FAIL"
  hFlush stdout

  putStr $ "Verifying assertions of:  " ++ name ++ " ... "
  hFlush stdout
  (_, result, _) <- readProcessWithExitCode "acl2" [] acl2Asm
  let pass = expected == (not $ any (isPrefixOf "ACL2 Error") $ lines result)
  putStrLn $ if pass then "pass" else "FAIL"
  writeFile (name ++ "_assertions.log") result
  hFlush stdout

  return $ terminates && pass

  where
  (name, acl2Asm', _) = compileModule m
  acl2Asm = unlines $ map show acl2Asm'

-- | Verifies a list of modules.
verifyModules :: [(Bool, Module)] -> IO Bool
verifyModules m = do
  pass <- sequence [ verifyModule a b | (a, b) <- m ]
  return $ and pass

-- | Verifies termination of a module.
verifyTermination :: Module -> IO Bool
verifyTermination m = do
  writeFile (name ++ ".lisp") acl2CPS
  (_, result, _) <- readProcessWithExitCode "acl2" [] acl2CPS
  let terminates = not $ any (isPrefixOf "ACL2 Error") $ lines result
  writeFile (name ++ "_termination.log") result
  return terminates
  where
  (name, _, acl2CPS') = compileModule m
  acl2CPS = unlines $ map show acl2CPS'

