-- | Compile CPS to RTL.
module Mira.RTLConvert
  ( rtlConvert
  ) where

import Data.List

import Mira.CPS as C
import Mira.RTL hiding (RTL, Var)
import qualified Mira.RTL as R

type RTL = R.RTL [Proc]

-- | Convert a list of CPS procedures converted with an explicit stack to an RTL program.
rtlConvert :: [Proc] -> Program
rtlConvert procs = snd $ elaborate procs $ do
  if hasMain
    then do
      label "start"
      pushCont "done"
      jump "main"
      mapM_ proc procs
      label "done"
      halt
    else do
      label "start"
      halt
      mapM_ proc procs
  where
  hasMain = not $ null [ () | Proc "main" _ _ _ <- procs ]

proc :: Proc -> RTL ()
proc (Proc name args _ body) = do
  comment $ "Procedure: " ++ name ++ "(" ++ intercalate ", " args ++ ")"
  label name
  cont body

cont :: Cont -> RTL ()
cont a = case a of
  Call f args (Just k) -> do
    procs <- getMeta
    let argVars = head [ args | Proc name args _ _ <- procs, name == f ] 
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

  Call f args Nothing -> do
    procs <- getMeta
    let argVars = head [ args | Proc name args _ _ <- procs, name == f ] 
    comment "Copy the arguments to the functions argument variables."
    sequence_ [ copy a b | (a, b) <- zip args argVars ]
    comment "Call the function."
    jump f

  C.Return Nothing  -> return'
  C.Return (Just a) -> copy a "retval" >> return'

  C.Push a b -> push a >> cont b

  Let a b c -> do
    case b of
      Var         b      -> copy  b a
      Literal     b      -> const' b a
      Deref       _      -> undefined'
      Alloc              -> undefined'
      Array       _      -> undefined'
      Struct      _      -> undefined'
      ArrayIndex  _ _    -> undefined'
      StructIndex _ _    -> undefined'
      C.Pop              -> pop a
      C.Intrinsic i args -> intrinsic i args a
    cont c

  Store _ _ _ -> undefined' -- cont $ Let a (Var b) c  --XXX Probably not correct.

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

