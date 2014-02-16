-- | Compile CPS to RTL.
module Ivory.Compile.ACL2.RTLConvert
  ( rtlConvert
  ) where

import Data.List

import Ivory.Compile.ACL2.CPS as C
import Ivory.Compile.ACL2.RTL hiding (RTL, Var)
import qualified Ivory.Compile.ACL2.RTL as R

type RTL i = R.RTL [Proc i] i

-- | Convert a list of CPS procedures to an RTL program.
rtlConvert :: [Proc i] -> Program i
rtlConvert procs = snd $ elaborate procs $ mapM_ proc $ alphaConvert procs

proc :: Proc i -> RTL i ()
proc (Proc name args body) = do
  comment $ "Procedure: " ++ name ++ "(" ++ intercalate ", " args ++ ")"
  label name
  cont body

cont :: Cont i -> RTL i ()
cont a = case a of
  Call f args k -> do
    procs <- getMeta
    let names = head [ args | Proc name args _ <- procs, name == f ] 
        vars  = contFreeVars k
    sequence_ [ push a | a <- vars ]
    kLabel <- gensym
    pushCont kLabel $ length vars
    sequence_ [ copy a $ sValue b | (a, b) <- zip names args ]
    jump f
    label kLabel
    sequence_ [ pop a | a <- reverse vars ]
    cont k

  {-
  Pop a -> do

    cont i a
    -}

  C.Return a -> case a of
    Just (Var a)     -> do copy "retval" a >> return'
    Just ReturnValue -> do                    return'
    Nothing          -> do                    return'

  Let a b c -> do
    case b of
      SValue    (Var b)     -> copy  a b
      SValue    ReturnValue -> copy  a "retval"
      Literal   b           -> const' a b
      C.Intrinsic i args    -> intrinsic i a $ map sValue args
      Deref     _ -> error "Deref not supported."
    cont c

  If a b c -> do
    onTrue <- gensym
    branch (sValue a) onTrue
    cont c
    label onTrue
    cont b

  C.Halt -> halt

  C.Assert a b -> do
    ok <- gensym
    branch (sValue a) ok
    fail'
    label ok
    cont b
  Assume a b -> cont $ Assert a b
  --Store   a b c -> sValue a ++ sValue b ++ cont i c
  --Forever a     -> cont i a
  --Loop    a b _ c d e -> sValue b ++ sValue c ++ cont (a : i) d ++ cont i e

sValue :: SValue -> Var
sValue a = case a of
  Var a -> a
  ReturnValue -> "retval"

