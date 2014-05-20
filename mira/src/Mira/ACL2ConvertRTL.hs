-- | Compile RTL to ACL2.
module Mira.ACL2ConvertRTL
  ( acl2ConvertRTL
  , showLit
  ) where

import Data.Maybe (fromJust)

import Mira.ACL2
import Mira.RTL
import Mira.Expr hiding (Expr (..))

acl2ConvertRTL :: Program -> [Expr]
acl2ConvertRTL program@(Program instrs) = utils ++ instructionSemantics ++
  [ step
  , stepN
  , defconst "*rtl-init-state*" $ quote $ obj [obj $ map (assembleInstruction labs vars) instrs, nil, nil, labs "start"]
  ] ++
  [ defun  ("fail-at-" ++ show a ++ "-fun") ["n"] $ not' $ equal (fromIntegral a) $ getPC $ stepN' n | a <- fails ] ++
  [ defthm ("fail-at-" ++ show a ++ "-thm") $ call ("fail-at-" ++ show a ++ "-fun") [n]              | a <- fails ]
  where
  labs :: Label -> Expr
  labs l = case lookup l [ (a, fromIntegral b) | (a, b) <- labels program ] of
    Nothing -> error $ "Label not found: " ++ l
    Just a  -> a
  vars :: Var -> Int
  vars = fromJust . flip lookup (zip (variables program) [0 ..])
  fails :: [Int]
  fails = [ a | (a, Fail) <- zip [0 ..] instrs ]
  stepN' n = call "rtl-step-n" [var "*rtl-init-state*", n]
  n = var "n"

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
    [ (fromIntegral $ encodeIntrinsic a, intrinsicACL2 a arg) | a <- allIntrinsics ] 0
  ]
  where
  s = var "s"
  a = var "a"
  b = var "b"
  c = var "c"
  mem = var "mem"
  arg i = nth (nth (fromIntegral i) b) mem

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
assembleInstruction :: (Label -> Expr) -> (Var -> Int) -> Instruction -> Expr
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
  Intrinsic a b c -> obj [codeIntrinsic, fromIntegral $ encodeIntrinsic a, obj (map varAddr b), varAddr c]
  Undefined       -> lit "undefined"
  where
  varAddr = fromIntegral . varAddr'

