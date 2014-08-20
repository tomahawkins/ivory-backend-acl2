-- | A generic assembly language.
module Mira.RTL
  ( Var
  , Label
  , Program     (..)
  , Instruction (..)
  , variables
  , labels
  -- * RTL DSL
  , RTL
  , elaborate
  , getMeta
  , setMeta
  , genVar
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
  , undefined'
  ) where

import Data.List
import MonadLib hiding (Label, jump)
import Text.Printf

import Mira.Expr hiding (Expr (Intrinsic))

type Label = String
data Program = Program [Instruction]

data Instruction
  = Comment   String       -- ^ A comment.
  | Label     Label        -- ^ Label a section of code.
  | Return                 -- ^ Pop label off of call stack and jump to address.
  | Jump      Label        -- ^ Jump to a label.
  | Branch    Var Label    -- ^ Jump to a label if var is true.
  | Fail                   -- ^ Assert that the program should never get here.
  | Halt                   -- ^ Halt the program.
  | Copy      Var Var      -- ^ Copy data from one var to another.
  | Push      Var          -- ^ Push a value onto the data stack.
  | PushCont  Label        -- ^ Push onto the call stack a label and the number words pushed onto the data stack.
  | Pop       Var          -- ^ Pop a value off the stack.
  | Const     Literal Var  -- ^ Load a literal into a var.
  | Intrinsic Intrinsic [Var] Var  -- ^ Call an intrinsic and assign result to var.
  | Undefined

instance Show Program where
  show (Program p) = unlines $ map show p

instance Show Instruction where
  show a = case a of
    Comment   a     -> printf "\t# %s" a
    Label     a     -> printf "%s:" a
    Return          ->        "\treturn'"
    Jump      a     -> printf "\tjump %s" a
    Branch    a b   -> printf "\tbranch %s %s" a b
    Fail            ->        "\tfail"
    Halt            ->        "\thalt"
    Copy      a b   -> printf "\tcopy %s %s" a b
    Push      a     -> printf "\tpush %s" a
    PushCont  a     -> printf "\tpushCont %s" a
    Pop       a     -> printf "\tpop  %s" a
    Const     a b   -> printf "\tconst' (%s) %s" (show a) b
    Intrinsic i a b -> printf "\tintrinsic (%s) (%s) %s" (show i) (intercalate ", " a) b
    Undefined       ->        "\tundefined"

-- | All the variables in a program.
variables :: Program -> [Var]
variables (Program p) = nub $ concatMap f p
  where
  f :: Instruction -> [Var]
  f a = case a of
    Comment   _     -> []
    Label     _     -> []
    Return          -> []
    Jump      _     -> []
    Branch    a _   -> [a]
    Fail            -> []
    Halt            -> []
    Copy      a b   -> [a, b]
    Push      a     -> [a]
    PushCont  _     -> []
    Pop       a     -> [a]
    Const     _ a   -> [a]
    Intrinsic _ a b -> a ++ [b]
    Undefined       -> []

-- | The address of all labels in a program.
labels :: Program -> [(Label, Int)]
labels (Program p) = [ (a, addr) | (Label a, addr) <- zip p [0 ..] ]


type RTL a = StateT (Int, a, Program) Id

-- | Elaborates an RTL description to a Program.
elaborate :: a -> RTL a () -> (a, Program)
elaborate a p = (a', b)
  where
  ((), (_, a', b)) = runId $ runStateT (0, a, Program []) p 

-- | Get the meta data.
getMeta :: RTL a a
getMeta = do
  (_, a, _) <- get
  return a

-- | Set the meta data.
setMeta :: a -> RTL a ()
setMeta a = do
  (i, _, p) <- get
  set (i, a, p)

-- | Generate a variable.
genVar :: RTL a String
genVar = do
  (i, a, p) <- get
  set (i + 1, a, p)
  return $ "_rtl_" ++ show i

instr :: Instruction -> RTL a ()
instr instr = do
  (i, a, Program p) <- get
  set (i, a, Program $ p ++ [instr])

comment :: String -> RTL a ()
comment = instr . Comment

label :: Label -> RTL a ()
label = instr . Label

return' :: RTL a ()
return' = instr Return

jump :: Label -> RTL a ()
jump = instr . Jump

branch :: Var -> Label -> RTL a ()
branch a b = instr $ Branch a b

fail' :: RTL a ()
fail' = instr Fail

halt :: RTL a ()
halt = instr Halt

copy :: Var -> Var -> RTL a ()
copy a b = instr $ Copy a b

push :: Var -> RTL a ()
push = instr . Push

pushCont :: Label -> RTL a ()
pushCont a = instr $ PushCont a

pop :: Var -> RTL a ()
pop = instr . Pop

const' :: Literal -> Var -> RTL a ()
const' a b = instr $ Const a b

intrinsic :: Intrinsic -> [Var] -> Var -> RTL a ()
intrinsic a b c = instr $ Intrinsic a b c

undefined' :: RTL a ()
undefined' = instr $ Undefined

