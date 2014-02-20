-- | Compile CPS to ACL2.
module Ivory.Compile.ACL2.ACL2Convert
  ( acl2Convert
  ) where

import Data.Maybe (fromJust)

import Ivory.Compile.ACL2.ACL2
import Ivory.Compile.ACL2.RTL
import Ivory.Language.Syntax.AST (ExpOp (..))

acl2Convert :: Program ExpOp -> [Expr]
acl2Convert program = utils ++ instructionSemantics ++ [step, stepN] ++ [assemble labs vars program]
  where
  labs :: Label -> Expr
  labs = fromJust . flip lookup [ (a, fromIntegral b) | (a, b) <- labels program ]
  vars :: Var -> Int
  vars = fromJust . flip lookup (zip (variables program) [0 ..])

-- State: '(instrMem dataMem callStack dataStack pc)
utils :: [Expr]
utils =
  [ defun "get-instr-mem"   ["s"]      $ nth' 0 s
  , defun "get-data-mem"    ["s"]      $ nth' 1 s
  , defun "set-data-mem"    ["s", "a"] $ replace' 1 s a
  , defun "get-call-stack"  ["s"]      $ nth' 2 s
  , defun "set-call-stack"  ["s", "a"] $ replace' 2 s a
  , defun "get-data-stack"  ["s"]      $ nth' 3 s
  , defun "set-data-stack"  ["s", "a"] $ replace' 3 s a
  , defun "get-pc"          ["s"]      $ nth' 4 s
  , defun "set-pc"          ["s", "a"] $ replace' 4 s a
  , defun "incr-pc"         ["s"]      $ setPC s $ getPC s + 1
  , defun "instr-fetch"     ["s"]      $ nth (getPC s) $ getInstrMem s
  , defun "push-data-stack" ["s", "a"] $ setDataStack s $ cons a $ getDataStack s
  , defun "pop-data-stack"  ["s"]      $ cons (setDataStack s $ cdr $ getDataStack s) (car $ getDataStack s)
  , defun "push-call-stack" ["s", "a"] $ setCallStack s $ cons a $ getCallStack s
  , defun "pop-call-stack"  ["s"]      $ cons (setCallStack s $ cdr $ getCallStack s) (car $ getCallStack s)
  , defun "replace" ["n", "l", "v"] $ if' (zp n) (cons v $ cdr l) (cons (car l) $ replace (n - 1) (cdr l) v)
  ]
  where
  s = var "s"
  a = var "a"
  n = var "n"
  l = var "l"
  v = var "v"

-- | Compile time nth function.
nth' :: Int -> Expr -> Expr
nth' n l
  | n <= 0    = car l
  | otherwise = nth' (n - 1) $ cdr l

-- | Replaces the nth element in a list with a value.
replace' n l v = f n l
  where
  f n l
    | n <= 0    = cons v $ cdr l
    | otherwise = cons (car l) $ f (n - 1) $ cdr l

replace n l v = call "replace" [n, l, v]

getInstrMem   s   = call "get-instr-mem"   [s]
getDataMem    s   = call "get-data-mem"    [s]
setDataMem    s a = call "set-data-mem"    [s, a]
getCallStack  s   = call "get-call-stack"  [s]
setCallStack  s a = call "set-call-stack"  [s, a]
getDataStack  s   = call "get-data-stack"  [s]
setDataStack  s a = call "set-data-stack"  [s, a]
getPC         s   = call "get-pc"          [s]
setPC         s a = call "set-pc"          [s, a]
pushCallStack s a = call "push-call-stack" [s, a]
popCallStack  s   = call "pop-call-stack"  [s]
pushDataStack s a = call "push-data-stack" [s, a]
popDataStack  s   = call "pop-data-stack"  [s]
incrPC        s   = call "incr-pc"         [s]


instructionSemantics :: [Expr]
instructionSemantics =
  [ defun "rtl-comment"   ["s"]                $ incrPC s
  , defun "rtl-label"     ["s"]                $ incrPC s
  , defun "rtl-return"    ["s"]                $ let' [("a", popCallStack s)] $ setPC (car a) (cdr a)
  , defun "rtl-jump"      ["s", "a"]           $ setPC s a
  , defun "rtl-fail"      ["s"]                $ s
  , defun "rtl-halt"      ["s"]                $ s
  , defun "rtl-branch"    ["s", "a", "b"]      $ if' (zip' $ nth a (getDataMem s)) (incrPC s) (setPC s b)
  , defun "rtl-push-cont" ["s", "a"]           $ incrPC $ pushCallStack s a
  , defun "rtl-push"      ["s", "a"]           $ incrPC $ pushDataStack s $ nth a $ getDataMem s 
  , defun "rtl-pop"       ["s", "a"]           $ incrPC $ let' [("b", popDataStack s), ("s", car b), ("b", cdr b)] $ setDataMem s $ replace a (getDataMem s) b 
  , defun "rtl-copy"      ["s", "a", "b"]      $ incrPC $ setDataMem s $ replace b (getDataMem s) $ nth a $ getDataMem s
  , defun "rtl-const"     ["s", "a", "b"]      $ incrPC $ setDataMem s $ replace b (getDataMem s) a
  , defun "rtl-intrinsic" ["s", "a", "b", "c"] $ incrPC $ setDataMem s $ replace c (getDataMem s) $ case' a
    [ (undefined', undefined')
    ] 0
  ]
  where
  s = var "s"
  a = var "a"
  b = var "b"
  c = var "c"

-- | Step the machine by one instruction.
step :: Expr
step = defun "rtl-step" ["s"]
  $ let' [("instr", call "instr-fetch" [s])]
  $ case' (car instr)
    [ (codeComment   , call "rtl-comment"   [s])
    , (codeLabel     , call "rtl-label"     [s])
    , (codeReturn    , call "rtl-return"    [s])
    , (codeFail      , call "rtl-fail"      [s])
    , (codeHalt      , call "rtl-halt"      [s])
    , (codeJump      , call "rtl-jump"      [s, nth 1 instr])
    , (codeBranch    , call "rtl-branch"    [s, nth 1 instr, nth 2 instr])
    , (codeCopy      , call "rtl-copy"      [s, nth 1 instr, nth 2 instr])
    , (codePush      , call "rtl-push"      [s, nth 1 instr])
    , (codePushCont  , call "rtl-push-cont" [s, nth 1 instr])
    , (codePop       , call "rtl-pop"       [s, nth 1 instr])
    , (codeConst     , call "rtl-const"     [s, nth 1 instr, nth 2 instr])
    , (codeIntrinsic , call "rtl_intrinsic" [s, nth 1 instr, nth 2 instr, nth 3 instr])
    ] s
  where
  s = var "s"
  instr = var "instr"

-- | Run the machine n number of steps.
stepN :: Expr
stepN = defun "rtl-step-n" ["s", "n"] $ if' (zp n) s (call "rtl-step-n" [call "rtl-step" [s], n - 1])
  where
  s = var "s"
  n = var "n"

-- | Assemble an RTL program into an ACL2 data structure.
assemble :: (Label -> Expr) -> (Var -> Int) -> Program ExpOp -> Expr
assemble labs vars (Program instrs) = defconst "rtl-program" $ quote $ obj $ map (encodeInstruction labs vars) instrs

codeComment   = 0
codeLabel     = 1
codeReturn    = 2
codeFail      = 3
codeHalt      = 4
codeJump      = 5
codeBranch    = 6
codeCopy      = 7
codePush      = 8
codePushCont  = 9
codePop       = 10
codeConst     = 11
codeIntrinsic = 12

encodeInstruction :: (Label -> Expr) -> (Var -> Int) -> Instruction ExpOp -> Expr
encodeInstruction labelAddr varAddr' a = case a of
  Comment   _     -> obj [codeComment                              ]
  Label     _     -> obj [codeLabel                                ]
  Return          -> obj [codeReturn                               ]
  Fail            -> obj [codeFail                                 ]
  Halt            -> obj [codeHalt                                 ]
  Jump      a     -> obj [codeJump,      labelAddr a               ]
  Branch    a b   -> obj [codeBranch,    varAddr a,    labelAddr b ]
  Copy      a b   -> obj [codeCopy,      varAddr a,    varAddr b   ]
  Push      a     -> obj [codePush,      varAddr a                 ]
  Pop       a     -> obj [codePop,       varAddr a                 ]
  PushCont  a     -> obj [codePushCont,  labelAddr a               ]
  Const     a b   -> obj [codeConst,     lit $ show a, varAddr b   ]
  Intrinsic a b c -> obj [codeIntrinsic, encodeOp a,   obj (map varAddr b), varAddr c]
  where
  varAddr = fromIntegral . varAddr'

encodeOp :: ExpOp -> Expr
encodeOp a = case a of
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

