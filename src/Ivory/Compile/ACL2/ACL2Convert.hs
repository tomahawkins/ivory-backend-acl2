-- | Compile CPS to ACL2.
module Ivory.Compile.ACL2.ACL2Convert
  ( acl2Convert
  ) where

import Data.Maybe (fromJust)

import Ivory.Compile.ACL2.ACL2
import Ivory.Compile.ACL2.RTL
import Ivory.Compile.ACL2.CPS (Literal (..))
import Ivory.Language.Syntax.AST (ExpOp (..))

acl2Convert :: Program ExpOp -> [Expr]
acl2Convert program@(Program instrs) = utils ++ instructionSemantics ++
  [ step
  , stepN
  , defconst "*rtl-init-state*" $ quote $ obj [obj $ map (assembleInstruction labs vars) instrs, nil, nil, labs "main"]
  ] ++
  [ defthm ("fail-at-" ++ show failAddr) $ not' $ equal (fromIntegral failAddr) $ getPC $ call "rtl-step-n" [var "*rtl-init-state*", var "n"]
  | failAddr <- fails
  ]
  where
  labs :: Label -> Expr
  labs = fromJust . flip lookup [ (a, fromIntegral b) | (a, b) <- labels program ]
  vars :: Var -> Int
  vars = fromJust . flip lookup (zip (variables program) [0 ..])
  fails :: [Int]
  fails = [ a | (a, Fail) <- zip [0 ..] instrs ]

-- State: '(instrMem dataMem callStack dataStack pc)
utils :: [Expr]
utils =
  [ defun "get-instr-mem"   ["s"]      $ nth' 0 s
  , defun "get-data-mem"    ["s"]      $ nth' 1 s
  , defun "set-data-mem"    ["s", "a"] $ replaceNth' 1 s a
  , defun "get-call-stack"  ["s"]      $ nth' 2 s
  , defun "set-call-stack"  ["s", "a"] $ replaceNth' 2 s a
  , defun "get-data-stack"  ["s"]      $ nth' 3 s
  , defun "set-data-stack"  ["s", "a"] $ replaceNth' 3 s a
  , defun "get-pc"          ["s"]      $ nth' 4 s
  , defun "set-pc"          ["s", "a"] $ replaceNth' 4 s a
  , defun "incr-pc"         ["s"]      $ setPC s $ getPC s + 1
  , defun "instr-fetch"     ["s"]      $ nth (getPC s) $ getInstrMem s
  , defun "push-data-stack" ["s", "a"] $ setDataStack s $ cons a $ getDataStack s
  , defun "pop-data-stack"  ["s"]      $ cons (setDataStack s $ cdr $ getDataStack s) (car $ getDataStack s)
  , defun "push-call-stack" ["s", "a"] $ setCallStack s $ cons a $ getCallStack s
  , defun "pop-call-stack"  ["s"]      $ cons (setCallStack s $ cdr $ getCallStack s) (car $ getCallStack s)
  , defun "replace-nth"     ["n", "l", "v"] $ if' (zp n) (cons v $ cdr l) (cons (car l) $ replaceNth (n - 1) (cdr l) v)
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
replaceNth' n l v = f n l
  where
  f n l
    | n <= 0    = cons v $ cdr l
    | otherwise = cons (car l) $ f (n - 1) $ cdr l

replaceNth n l v = call "replace-nth" [n, l, v]

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
  , defun "rtl-pop"       ["s", "a"]           $ incrPC $ let' [("b", popDataStack s), ("s", car b), ("b", cdr b)] $ setDataMem s $ replaceNth a (getDataMem s) b 
  , defun "rtl-copy"      ["s", "a", "b"]      $ incrPC $ setDataMem s $ replaceNth b (getDataMem s) $ nth a $ getDataMem s
  , defun "rtl-const"     ["s", "a", "b"]      $ incrPC $ setDataMem s $ replaceNth b (getDataMem s) a
  , defun "rtl-intrinsic" ["s", "a", "b", "c"] $ incrPC $ setDataMem s $ let' [("mem", getDataMem s)] $ replaceNth c mem $ case' a
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
    {-
    , (codeExpDiv           , )
    , (codeExpMod           , )
    , (codeExpRecip         , )
    , (codeExpFExp          , )
    , (codeExpFSqrt         , )
    , (codeExpFLog          , )
    , (codeExpFPow          , )
    , (codeExpFLogBase      , )
    , (codeExpFSin          , )
    , (codeExpFTan          , )
    , (codeExpFCos          , )
    , (codeExpFAsin         , )
    , (codeExpFAtan         , )
    , (codeExpFAcos         , )
    , (codeExpFSinh         , )
    , (codeExpFTanh         , )
    , (codeExpFCosh         , )
    , (codeExpFAsinh        , )
    , (codeExpFAtanh        , )
    , (codeExpFAcosh        , )
    , (codeExpIsNan         , )
    , (codeExpIsInf         , )
    , (codeExpRoundF        , )
    , (codeExpCeilF         , )
    , (codeExpFloorF        , )
    , (codeExpToFloat       , )
    , (codeExpFromFloat     , )
    , (codeExpBitAnd        , )
    , (codeExpBitOr         , )
    , (codeExpBitXor        , )
    , (codeExpBitComplement , )
    , (codeExpBitShiftL     , )
    , (codeExpBitShiftR     , )
    -}
    ] 0
  ]
  where
  s = var "s"
  a = var "a"
  b = var "b"
  c = var "c"
  mem = var "mem"
  arg i = nth (nth i b) mem

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
    , (codeIntrinsic , call "rtl-intrinsic" [s, nth 1 instr, nth 2 instr, nth 3 instr])
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

-- | Assemble an intruction into ACL2.
assembleInstruction :: (Label -> Expr) -> (Var -> Int) -> Instruction ExpOp -> Expr
assembleInstruction labelAddr varAddr' a = case a of
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
  Const     a b   -> obj [codeConst,     lit $ showLit a, varAddr b   ]
  Intrinsic a b c -> obj [codeIntrinsic, encodeOp a,   obj (map varAddr b), varAddr c]
  where
  varAddr = fromIntegral . varAddr'

showLit :: Literal -> String
showLit a = case a of
  LitInteger a -> show a
  LitBool    True  -> "1"
  LitBool    False -> "0"
  a -> error $ "unsupported literal: " ++ show a

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

