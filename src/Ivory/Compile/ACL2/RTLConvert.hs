-- | Compile CPS to RTL.
module Ivory.Compile.ACL2.RTLConvert
  ( rtlConvert
  ) where

import Data.List

import Ivory.Compile.ACL2.CPS as C
import Ivory.Compile.ACL2.RTL hiding (RTL, Var)
import qualified Ivory.Compile.ACL2.RTL as R

type RTL i = R.RTL [Proc i] i

-- | Convert a list of CPS procedures converted with an explicit stack to an RTL program.
rtlConvert :: [Proc i] -> Program i
rtlConvert procs = snd $ elaborate procs $ do
  label "start"
  pushCont "done"
  jump "main"
  mapM_ proc procs
  label "done"
  halt

proc :: Proc i -> RTL i ()
proc (Proc name args body) = do
  comment $ "Procedure: " ++ name ++ "(" ++ intercalate ", " args ++ ")"
  label name
  cont body

cont :: Cont i -> RTL i ()
cont a = case a of
  Call f args k -> do
    procs <- getMeta
    let argVars = head [ args | Proc name args _ <- procs, name == f ] 
    kLabel <- genVar
    comment "Push the continuation."
    pushCont kLabel
    comment "Copy the arguments to the functions argument variables."
    sequence_ [ copy a b | (a, b) <- zip args argVars ]
    comment "Call the function."
    jump f
    comment "On return from function..."
    label kLabel
    comment "Execute the continuation after the call."
    cont k

  C.Return Nothing  -> return'
  C.Return (Just a) -> copy a "retval" >> return'

  C.Push a b -> push a >> cont b

  Let a b c -> do
    case b of
      Var b              -> copy  b a
      Literal b          -> const' b a
      C.Pop              -> pop a
      C.Intrinsic i args -> intrinsic i args a
    cont c

  If a b c -> do
    onTrue <- genVar
    branch a onTrue
    cont c
    label onTrue
    cont b

  C.Halt -> halt

  C.Assert a b -> do
    ok <- genVar
    branch a ok
    fail'
    label ok
    cont b

  Assume a b -> cont $ Assert a b

