-- | Compile CPS to ACL2.
module Ivory.Compile.ACL2.ACL2Convert
  ( acl2Convert
  ) where

import Ivory.Compile.ACL2.ACL2
import Ivory.Compile.ACL2.RTL
import Ivory.Language.Syntax.AST (ExpOp (..))

acl2Convert :: Program ExpOp -> [Expr]
acl2Convert _ = utils ++ instructionSemantics
  --where
  --vars = variables p
  --labs = labels    p

-- State: (list instrMem dataMem callStack dataStack pc)
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
  , defun "incr-pc"         ["s"]      $ setPC s $ add (lit "1") $ getPC s
  , defun "instr-fetch"     ["s"]      $ nth (getPC s) $ getInstrMem s
  , defun "push-data"       ["s", "a"] $ setDataStack s $ cons a $ getDataStack s
  , defun "pop-data"        ["s"]      $ cons (setDataStack s $ cdr $ getDataStack s) (car $ getDataStack s)
  , defun "push-call-stack" ["s", "a"] $ setCallStack s $ cons a $ getCallStack s
  , defun "pop-call-stack"  ["s"]      $ cons (setCallStack s $ cdr $ getCallStack s) (car $ getCallStack s)
  ]
  where
  s      = var "s"
  a      = var "a"
  nth' i l
    | i <= 0    = car l
    | otherwise = nth' (i - 1) $ cdr l
  replace n l field = f n l
    where
    f n l
      | n <= 0    = cons field $ cdr l
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
incrPC        s   = call "incr-pc"         [s]

instructionSemantics :: [Expr]
instructionSemantics =
  [ defun "rtl-comment"   ["s"]      $ incrPC s
  , defun "rtl-label"     ["s"]      $ incrPC s
  , defun "rtl-return"    ["s"]      $ let' [("a", popCallStack s)] $ setPC (car a) (cdr a)
  , defun "rtl-jump"      ["s", "a"] $ setPC s a
  , defun "rtl-fail"      ["s"]      $ s
  , defun "rtl-halt"      ["s"]      $ s
  , defun "rtl-push-cont" ["s", "a"] $ incrPC $ pushCallStack s a
  ]
  where
  s = var "s"
  a = var "a"

  {-
  = Comment   String       -- ^ A comment.
  | Label     Label        -- ^ Label a section of code.
  | Return                 -- ^ Pop label off of call stack and jump to address.
  | Jump      Label        -- ^ Jump to a label.
  | Branch    Var Label    -- ^ Jump to a label if var is true.
  | Fail                   -- ^ Assert that the program should never get here.
  | Halt                   -- ^ Halt the program.
  | Copy      Var Var      -- ^ Copy data from one var to another.
  | Push      Var          -- ^ Push a value onto the data stack.
  | PushCont  Label        -- ^ Push onto the call stack a label to return to.
  | Pop       Var          -- ^ Pop a value off the stack.
  | Const     Literal Var  -- ^ Load a literal into a var.
  | Intrinsic i [Var] Var  -- ^ Call an intrinsic and assign result to var.
  -}

