-- | Compile CPS to RTL.
module Ivory.Compile.ACL2.RTLConvert
  ( rtlConvert
  ) where

import Data.List

import Ivory.Compile.ACL2.CPS as C
import Ivory.Compile.ACL2.RTL hiding (RTL, Var)
import qualified Ivory.Compile.ACL2.RTL as R

type RTL i = R.RTL [Proc i] i

-- | Convert a list of alpha-converted CPS procedures to an RTL program.
rtlConvert :: [Proc i] -> Program i
rtlConvert procs = snd $ elaborate procs $ mapM_ proc procs

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
        vars  = contFreeVars k
    comment "Pushing variables that are needed for after the call returns."
    sequence_ [ push a | a <- vars ]
    kLabel <- genVar
    comment "Push the continuation."
    pushCont kLabel
    comment "Copy the arguments to the functions argument variables."
    sequence_ [ copy a b | (a, b) <- zip args argVars ]
    comment "Call the function."
    jump f
    comment "On return from function..."
    label kLabel
    comment "Restore needed variables."
    sequence_ [ pop a | a <- reverse vars ]
    comment "Execute the continuation after the call."
    cont k

  C.Return Nothing  -> return'
  C.Return (Just a) -> copy a "retval" >> return'

  Let a b c -> do
    case b of
      Var b              -> copy  b a
      Literal b          -> const' b a
      C.Intrinsic i args -> intrinsic i args a
      Deref _            -> error "Deref not supported."
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

  C.Pop _          -> error "Pop not supported."
  Store _ _ _      -> error "Store not supported."
  Forever _ _      -> error "Forever not supported."
  Loop _ _ _ _ _ _ -> error "Loop not supported."

