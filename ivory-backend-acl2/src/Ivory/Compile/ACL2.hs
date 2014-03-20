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
  acl21 = acl2ConvertRTL intrinsicCode intrinsicImp rtl 
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

intrinsicCode :: ExpOp -> Expr
intrinsicCode a = case a of
  ExpEq  _         -> codeExpEq           
  ExpNeq _         -> codeExpNeq          
  ExpCond          -> codeExpCond         
  ExpGt False _    -> codeExpGt           
  ExpGt True  _    -> codeExpGe           
  ExpLt False _    -> codeExpLt           
  ExpLt True  _    -> codeExpLe           
  ExpNot           -> codeExpNot          
  ExpAnd           -> codeExpAnd          
  ExpOr            -> codeExpOr           
  ExpMul           -> codeExpMul          
  ExpAdd           -> codeExpAdd          
  ExpSub           -> codeExpSub          
  ExpNegate        -> codeExpNegate       
  ExpAbs           -> codeExpAbs          
  ExpSignum        -> codeExpSignum       
  a -> error $ "ExpOp not supported: " ++ show a
  {-
  ExpDiv           -> codeExpDiv          
  ExpMod           -> codeExpMod          
  ExpRecip         -> codeExpRecip        
  ExpFExp          -> codeExpFExp         
  ExpFSqrt         -> codeExpFSqrt        
  ExpFLog          -> codeExpFLog         
  ExpFPow          -> codeExpFPow         
  ExpFLogBase      -> codeExpFLogBase     
  ExpFSin          -> codeExpFSin         
  ExpFTan          -> codeExpFTan         
  ExpFCos          -> codeExpFCos         
  ExpFAsin         -> codeExpFAsin        
  ExpFAtan         -> codeExpFAtan        
  ExpFAcos         -> codeExpFAcos        
  ExpFSinh         -> codeExpFSinh        
  ExpFTanh         -> codeExpFTanh        
  ExpFCosh         -> codeExpFCosh        
  ExpFAsinh        -> codeExpFAsinh       
  ExpFAtanh        -> codeExpFAtanh       
  ExpFAcosh        -> codeExpFAcosh       
  ExpIsNan _       -> codeExpIsNan        
  ExpIsInf _       -> codeExpIsInf        
  ExpRoundF        -> codeExpRoundF       
  ExpCeilF         -> codeExpCeilF        
  ExpFloorF        -> codeExpFloorF       
  ExpToFloat   _   -> codeExpToFloat      
  ExpFromFloat _   -> codeExpFromFloat    
  ExpBitAnd        -> codeExpBitAnd       
  ExpBitOr         -> codeExpBitOr        
  ExpBitXor        -> codeExpBitXor       
  ExpBitComplement -> codeExpBitComplement
  ExpBitShiftL     -> codeExpBitShiftL    
  ExpBitShiftR     -> codeExpBitShiftR    
  -}

codeExpEq            = 0
codeExpNeq           = 1
codeExpCond          = 2
codeExpGt            = 3
codeExpGe            = 4
codeExpLt            = 5
codeExpLe            = 6
codeExpNot           = 7
codeExpAnd           = 8
codeExpOr            = 9
codeExpMul           = 10
codeExpAdd           = 11
codeExpSub           = 12
codeExpNegate        = 13
codeExpAbs           = 14
codeExpSignum        = 15
{-
codeExpDiv           = 16
codeExpMod           = 17
codeExpRecip         = 18
codeExpFExp          = 19
codeExpFSqrt         = 20
codeExpFLog          = 21
codeExpFPow          = 22
codeExpFLogBase      = 23
codeExpFSin          = 24
codeExpFTan          = 25
codeExpFCos          = 26
codeExpFAsin         = 27
codeExpFAtan         = 28
codeExpFAcos         = 29
codeExpFSinh         = 30
codeExpFTanh         = 31
codeExpFCosh         = 32
codeExpFAsinh        = 33
codeExpFAtanh        = 34
codeExpFAcosh        = 35
codeExpIsNan         = 36
codeExpIsInf         = 37
codeExpRoundF        = 38
codeExpCeilF         = 39
codeExpFloorF        = 40
codeExpToFloat       = 41
codeExpFromFloat     = 42
codeExpBitAnd        = 43
codeExpBitOr         = 44
codeExpBitXor        = 45
codeExpBitComplement = 46
codeExpBitShiftL     = 47
codeExpBitShiftR     = 48
-}

intrinsicImp :: Expr -> (Int -> Expr) -> Expr
intrinsicImp op arg = case' op
  [ (codeExpEq            , if' (equal (arg 0) (arg 1)) 1 0)
  , (codeExpNeq           , if' (not' (equal (arg 0) (arg 1))) 1 0)
  , (codeExpCond          , if' (zip' (arg 0)) (arg 2) (arg 1))
  , (codeExpGt            , if' (call ">"  [arg 0, arg 1]) 1 0)
  , (codeExpGe            , if' (call ">=" [arg 0, arg 1]) 1 0)
  , (codeExpLt            , if' (call "<"  [arg 0, arg 1]) 1 0)
  , (codeExpLe            , if' (call "<=" [arg 0, arg 1]) 1 0)
  , (codeExpNot           , if' (zip' (arg 0)) 1 0)
  , (codeExpAnd           , if' (or'  (zip' (arg 0)) (zip' (arg 1))) 0 1)
  , (codeExpOr            , if' (and' (zip' (arg 0)) (zip' (arg 1))) 0 1)
  , (codeExpMul           , arg 0 * arg 1)
  , (codeExpAdd           , arg 0 + arg 1)
  , (codeExpSub           , arg 0 - arg 1)
  , (codeExpNegate        , 0 - arg 1)
  , (codeExpAbs           , if' (call ">=" [arg 0, 0]) (arg 0) (0 - arg 0))
  , (codeExpSignum        , if' (call ">"  [arg 0, 0]) 1 $ if' (call "<" [arg 0, 0]) (-1) 0)
  ] 0

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

