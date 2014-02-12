-- | Compiling Ivory to ACL2.
module Ivory.Compile.ACL2
  ( compileModule
  ) where

import Data.List
import Text.Printf

import Ivory.Compile.ACL2.ACL2Convert
import Ivory.Compile.ACL2.CPSConvert
import qualified Ivory.Language.Syntax.AST as I
import Ivory.Language.Syntax.Names
import Ivory.Language.Syntax.Type

compileModule :: I.Module -> IO ()
compileModule m = do
  putStrLn $ showModule m
  mapM_ (putStrLn . showProc) $ procs m
  mapM_ (putStrLn . show . cpsConvertProc) $ procs m 
  mapM_ (putStrLn . show . acl2Convert . cpsConvertProc) $ procs m 

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

indent :: String -> String
indent = intercalate "\n" . map ("\t" ++) . lines

block :: String -> String
block a = "{\n" ++ indent a ++ "\n}\n"

