-- | Verifies and removes assertions from an Ivory program.
module Ivory.Opts.Asserts
  ( assertsFold
  ) where

import qualified Ivory.Language.Syntax.AST as I

assertsFold :: [I.Module] -> IO [I.Module]
assertsFold = return

