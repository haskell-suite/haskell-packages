{-# LANGUAGE StandaloneDeriving #-}
import Distribution.HaskellSuite.Cabal
import Distribution.HaskellSuite.PackageDB
import Distribution.HaskellSuite.Tool
import Data.Version
import Language.Haskell.Exts.Annotated.CPP

main = defaultMain $
  (simpleCompiler
    "myTool"
    (Version [3,1,4] [])
    []
    (\dir exts opts pkgdbs pkgs args -> print (dir, exts, opts, pkgdbs, pkgs, args))
    []
    :: SimpleCompiler (StandardDB Name))

data Name
instance IsDBName Name where getDBName = return "test-name"

deriving instance Show CpphsOptions
deriving instance Show BoolOptions
