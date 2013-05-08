module Distribution.HaskellSuite.Cabal
  ( defaultMain )
  where

import {-# SOURCE #-} qualified Distribution.HaskellSuite.Compiler as Compiler

defaultMain
  :: Compiler.Is c
  => c -> IO ()
