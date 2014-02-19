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
  , defun "set-data-mem"    ["s", "a"] $ replace 1 s a
  , defun "get-call-stack"  ["s"]      $ nth' 2 s
  , defun "set-call-stack"  ["s", "a"] $ replace 2 s a
  , defun "get-data-stack"  ["s"]      $ nth' 3 s
  , defun "set-data-stack"  ["s", "a"] $ replace 3 s a
  , defun "get-pc"          ["s"]      $ nth' 4 s
  , defun "set-pc"          ["s", "a"] $ replace 4 s a
  , defun "incr-pc"         ["s"]      $ setPC s $ getPC s + 1
  , defun "instr-fetch"     ["s"]      $ nth (getPC s) $ getInstrMem s
  , defun "push-data-stack" ["s", "a"] $ setDataStack s $ cons a $ getDataStack s
  , defun "pop-data-stack"  ["s"]      $ cons (setDataStack s $ cdr $ getDataStack s) (car $ getDataStack s)
  , defun "push-call-stack" ["s", "a"] $ setCallStack s $ cons a $ getCallStack s
  , defun "pop-call-stack"  ["s"]      $ cons (setCallStack s $ cdr $ getCallStack s) (car $ getCallStack s)
  ]
  where
  s      = var "s"
  a      = var "a"

-- | Compile time nth function.
nth' :: Int -> Expr -> Expr
nth' n l
  | n <= 0    = car l
  | otherwise = nth' (n - 1) $ cdr l

-- | Replaces the nth element in a list with a value.
replace n l v = f n l
  where
  f n l
    | n <= 0    = cons v $ cdr l
    | otherwise = cons (car l) $ f (n - 1) $ cdr l

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
  [ defun "rtl-comment"   ["s"]           $ incrPC s
  , defun "rtl-label"     ["s"]           $ incrPC s
  , defun "rtl-return"    ["s"]           $ let' [("a", popCallStack s)] $ setPC (car a) (cdr a)
  , defun "rtl-jump"      ["s", "a"]      $ setPC s a
  , defun "rtl-fail"      ["s"]           $ s
  , defun "rtl-halt"      ["s"]           $ s
  , defun "rtl-push-cont" ["s", "a"]      $ incrPC $ pushCallStack s a
  , defun "rtl-branch"    ["s", "a", "b"] $ if' (zip' $ nth a (getDataMem s)) (incrPC s) (setPC s b)
  , defun "rtl-push"      ["s", "a"]      $ incrPC $ pushDataStack s $ nth a $ getDataMem s 
  ]
  where
  s = var "s"
  a = var "a"
  b = var "b"

  {-
  | Copy      Var Var      -- ^ Copy data from one var to another.
  | Pop       Var          -- ^ Pop a value off the stack.
  | Const     Literal Var  -- ^ Load a literal into a var.
  | Intrinsic i [Var] Var  -- ^ Call an intrinsic and assign result to var.
  -}

-- | Step the machine by one instruction.
step :: Expr
step = defun "rtl-step" ["s"] $ undefined'
  where
  s = var "s"

-- | Run the machine n number of steps.
stepN :: Expr
stepN = defun "rtl-step-n" ["s", "n"] $ if' (zp n) s (call "rtl-step-n" [call "rtl-step" [s], n - 1])
  where
  s = var "s"
  n = var "n"

-- | Assemble an RTL program into an ACL2 data structure.
assemble :: (Label -> Expr) -> (Var -> Int) -> Program ExpOp -> Expr
assemble labs vars (Program instrs) = defconst "rtl-program" $ quote $ obj $ map (encodeInstruction labs vars) instrs

opcodeComment   = 0
opcodeLabel     = 1
opcodeReturn    = 2
opcodeFail      = 3
opcodeHalt      = 4
opcodeJump      = 5
opcodeBranch    = 6
opcodeCopy      = 7
opcodePush      = 8
opcodePushCont  = 9
opcodePop       = 10
opcodeConst     = 11
opcodeIntrinsic = 12

encodeInstruction :: (Label -> Expr) -> (Var -> Int) -> Instruction ExpOp -> Expr
encodeInstruction labelAddr varAddr' a = case a of
  Comment   _     -> obj [opcodeComment                              ]
  Label     _     -> obj [opcodeLabel                                ]
  Return          -> obj [opcodeReturn                               ]
  Fail            -> obj [opcodeFail                                 ]
  Halt            -> obj [opcodeHalt                                 ]
  Jump      a     -> obj [opcodeJump,      labelAddr a               ]
  Branch    a b   -> obj [opcodeBranch,    varAddr a,    labelAddr b ]
  Copy      a b   -> obj [opcodeCopy,      varAddr a,    varAddr b   ]
  Push      a     -> obj [opcodePush,      varAddr a                 ]
  Pop       a     -> obj [opcodePop,       varAddr a                 ]
  PushCont  a     -> obj [opcodePushCont,  labelAddr a               ]
  Const     a b   -> obj [opcodeConst,     lit $ show a, varAddr b   ]
  Intrinsic a b c -> obj [opcodeIntrinsic, encodeOp a,   obj (map varAddr b), varAddr c]
  where
  varAddr = fromIntegral . varAddr'

encodeOp :: ExpOp -> Expr
encodeOp _ = undefined'

