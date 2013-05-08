module Distribution.HaskellSuite.Cabal where

import {-# SOURCE #-} qualified Distribution.HaskellSuite.Compiler as Compiler

main
  :: Compiler.Is c
  => c -> IO ()
