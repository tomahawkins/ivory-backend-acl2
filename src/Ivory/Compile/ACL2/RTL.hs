-- | A generic assembly language.
module Ivory.Compile.ACL2.RTL
  ( Var
  , Label
  , Program     (..)
  , Instruction (..)
  -- * RTL DSL
  , RTL
  , elaborate
  , getMeta
  , setMeta
  , gensym
  -- ** Instructions
  , comment
  , label
  , return'
  , jump
  , branch
  , fail'
  , halt
  , copy
  , push
  , pushCont
  , pop
  , const'
  , intrinsic
  ) where

import MonadLib hiding (Label, jump)
import Text.Printf

import Ivory.Compile.ACL2.CPS (Literal)

type Var   = String
type Label = String
data Program i = Program [Instruction i]

data Instruction i
  = Comment   String       -- ^ A comment.
  | Label     Label        -- ^ Label a section of code.
  | Return                 -- ^ Pop label off of call stack and jump to address.
  | Jump      Label        -- ^ Jump to a label.
  | Branch    Var Label    -- ^ Jump to a label if var is true.
  | Fail                   -- ^ Assert that the program should never get here.
  | Halt                   -- ^ Halt the program.
  | Copy      Var Var      -- ^ Copy data from one var to another.
  | Push      Var          -- ^ Push a value onto the data stack.
  | PushCont  Label Int    -- ^ Push onto the call stack a label and the number words pushed onto the data stack.
  | Pop       Var          -- ^ Pop a value off the stack.
  | Const     Var Literal  -- ^ Load a literal into a var.
  | Intrinsic i Var [Var]  -- ^ Call an intrinsic and assign result to var.

instance Show i => Show (Program i) where
  show (Program p) = unlines $ map show p

instance Show i => Show (Instruction i) where
  show a = case a of
    Comment   a     -> printf "comment %s" $ show a
    Label     a     -> printf "%s:" a
    Return          ->        "\treturn'"
    Jump      a     -> printf "\tjump %s" a
    Branch    a b   -> printf "\tbranch %s %s" a b
    Fail            ->        "\tfail"
    Halt            ->        "\thalt"
    Copy      a b   -> printf "\tcopy %s %s" a b
    Push      a     -> printf "\tpush %s" a
    PushCont  a b   -> printf "\tpushCont %s %d" a b
    Pop       a     -> printf "\tpop  %s" a
    Const     a b   -> printf "\tconst' %s %s" a (show b)
    Intrinsic i a b -> printf "\tintrinsic %s %s %s" (show i) a (show b)

type RTL a i = StateT (Int, a, Program i) Id

elaborate :: a -> RTL a i () -> (a, Program i)
elaborate a p = (a', b)
  where
  ((), (_, a', b)) = runId $ runStateT (0, a, Program []) p 

getMeta :: RTL a i a
getMeta = do
  (_, a, _) <- get
  return a

setMeta :: a -> RTL a i ()
setMeta a = do
  (i, _, p) <- get
  set (i, a, p)

gensym :: RTL a i String
gensym = do
  (i, a, p) <- get
  set (i + 1, a, p)
  return $ "_rtl_" ++ show i

instr :: Instruction i -> RTL a i ()
instr instr = do
  (i, a, Program p) <- get
  set (i, a, Program $ p ++ [instr])

comment :: String -> RTL a i ()
comment = instr . Comment

label :: Label -> RTL a i ()
label = instr . Label

return' :: RTL a i ()
return' = instr Return

jump :: Label -> RTL a i ()
jump = instr . Jump

branch :: Var -> Label -> RTL a i ()
branch a b = instr $ Branch a b

fail' :: RTL a i ()
fail' = instr Fail

halt :: RTL a i ()
halt = instr Halt

copy :: Var -> Var -> RTL a i ()
copy a b = instr $ Copy a b

push :: Var -> RTL a i ()
push = instr . Push

pushCont :: Label -> Int -> RTL a i ()
pushCont a b = instr $ PushCont a b

pop :: Var -> RTL a i ()
pop = instr . Pop

const' :: Var -> Literal -> RTL a i ()
const' a b = instr $ Const a b

intrinsic :: i -> Var -> [Var] -> RTL a i ()
intrinsic a b c = instr $ Intrinsic a b c

