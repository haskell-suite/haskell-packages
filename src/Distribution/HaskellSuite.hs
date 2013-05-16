-- | This module re-exports all you need in order to /read/ package
-- databases and module info files created by compilers that use
-- haskell-packages.
--
-- If you are writing a compiler, i.e. a program that creates or writes
-- package databases or module info files — then take a look at
-- "Distribution.HaskellSuite.Compiler". It provides command-line
-- options handling and Cabal integration.
module Distribution.HaskellSuite
  ( module Distribution.HaskellSuite.Packages
  , module Distribution.HaskellSuite.Modules
  )
  where

import Distribution.HaskellSuite.Packages
import Distribution.HaskellSuite.Modules
