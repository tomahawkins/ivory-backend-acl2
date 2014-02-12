-- | Compiling Ivory to ACL2.
module Ivory.Compile.ACL2
  ( compileModule
  ) where

import Data.List
import Text.Printf

import qualified Ivory.Compile.ACL2.ACL2 as A
import Ivory.Compile.ACL2.CPS
import Ivory.Compile.ACL2.CPSConvert
import qualified Ivory.Language.Syntax.AST as I
import Ivory.Language.Syntax.Names
import Ivory.Language.Syntax.Type

compileModule :: I.Module -> IO ()
compileModule m = do
  putStrLn $ showModule m
  mapM_ (putStrLn . showProc) $ procs m
  mapM_ (putStrLn . show . cpsConvertProc) $ procs m 
  mapM_ (putStrLn . show . acl2 . cpsConvertProc) $ procs m 

showModule :: I.Module -> String
showModule m = unlines
  [ "modName        : " ++ (show $ I.modName        m)
  , "modHeaders     : " ++ (show $ I.modHeaders     m)
  , "modDepends     : " ++ (show $ I.modDepends     m)
  , "modExterns     : " ++ (show $ I.modExterns     m)
  , "modImports     : " ++ (show $ I.modImports     m)
  , "modProcs       : " ++ (show $ I.modProcs       m)
  , "modStructs     : " ++ (show $ I.modStructs     m)
  , "modAreas       : " ++ (show $ I.modAreas       m)
  , "modAreaImports : " ++ (show $ I.modAreaImports m)
  , "modSourceDeps  : " ++ (show $ I.modSourceDeps  m)
  ]

procs :: I.Module -> [I.Proc]
procs m = I.public (I.modProcs m) ++ I.private (I.modProcs m)

showProc :: I.Proc -> String
showProc p = unlines
  [ printf "%s %s(%s)" (show $ I.procRetTy p) (I.procSym p) (intercalate ", " $ map showArg (I.procArgs p))
  , block $ unlines $ map showStmt $ I.procBody p
  ]

showArg :: Typed Var -> String
showArg a = show (tType a) ++ " " ++ varSym (tValue a)

showStmt :: I.Stmt -> String
showStmt a = case a of
  I.IfTE a b c -> unlines
    [ printf "if (%s)" $ show a
    , block $ unlines $ map showStmt b
    , "else"
    , block $ unlines $ map showStmt c
    ]
  a -> show a

acl2 :: Proc -> A.Expr
acl2 (Proc name args body) = A.Defun name args $ acl2Cont body

acl2Cont :: Cont -> A.Expr
acl2Cont a = case a of
  Let     a b c -> A.Let' [(a, acl2BValue b)] $ acl2Cont c
{-
  = Call    Sym [SValue]       -- ^ Function call.
  | Push    Cont Cont          -- ^ Push a continuation onto the stack.
  | Pop     Cont               -- ^ Pop a continuation off the stack and throw it away. 
  | Return  (Maybe SValue)     -- ^ Pop a continuation off the stack and execute it.  Return value saved to ReturnValue.
  | Let     Sym BValue Cont
  | If      SValue Cont Cont
  | Halt
  | Assert  SValue Cont
  | Assume  SValue Cont
  | Store   SValue SValue Cont
  | Assign  Sym SValue Cont
  | Forever Cont
  | Loop    Sym SValue Incr SValue Cont Cont
  -}

acl2BValue :: BValue -> A.Expr
acl2BValue a = case a of
  Intrinsic   a args -> acl2Intrinsic a $ map acl2SValue args
  SValue      a -> acl2SValue a
  Deref _   -> error "acl2: Deref not supported."
  Pair  _ _ -> error "acl2: Pair not supported."

acl2SValue :: SValue -> A.Expr
acl2SValue a = case a of
  Sym a -> A.Var a
  ReturnValue -> A.Var "retval"
  Literal a -> case a of
    I.LitInteger a     -> A.Lit $ show a
    I.LitFloat   a     -> A.Lit $ show a
    I.LitDouble  a     -> A.Lit $ show a
    I.LitChar    a     -> A.Lit $ show a
    I.LitString  a     -> A.Lit $ show a
    I.LitBool    True  -> A.Lit "true"
    I.LitBool    False -> A.Lit "nil"
    I.LitNull          -> A.Lit "nil"

acl2Intrinsic :: Intrinsic -> [A.Expr] -> A.Expr
acl2Intrinsic (IntrinsicOp a) args = case (a, args) of
  (I.ExpEq _,        [a, b]) -> A.Equal a b
  (I.ExpNeq _,       [a, b]) -> A.Not $ A.Equal a b
  (I.ExpGt False _,  [a, b]) -> A.Gt a b
  (I.ExpGt True  _,  [a, b]) -> A.Ge a b
  (I.ExpLt False _,  [a, b]) -> A.Lt a b
  (I.ExpLt True  _,  [a, b]) -> A.Le a b
  (I.ExpAdd,         [a, b]) -> A.Add a b
  (I.ExpSub,         [a, b]) -> A.Sub a b
  (I.ExpNot,         [a]   ) -> A.Not a
  (I.ExpAnd,         [a, b]) -> A.And [a, b]
  (I.ExpOr,          [a, b]) -> A.Or  [a, b]
  (I.ExpCond,     [a, b, c]) -> A.Mux a b c

indent :: String -> String
indent = intercalate "\n" . map ("\t" ++) . lines

block :: String -> String
block a = "{\n" ++ indent a ++ "\n}\n"

