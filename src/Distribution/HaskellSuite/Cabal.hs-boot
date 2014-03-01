module Distribution.HaskellSuite.Cabal where

import {-# SOURCE #-} qualified Distribution.HaskellSuite.Compiler as Compiler
import Options.Applicative

main
  :: Compiler.Is c
  => c -> IO ()
customMain
  :: Compiler.Is c
  => Parser (IO ())
  -> c -> IO ()
