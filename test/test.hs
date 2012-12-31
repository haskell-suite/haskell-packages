{-# LANGUAGE StandaloneDeriving #-}
import Distribution.HaskellSuite.Cabal
import Distribution.HaskellSuite.Tool
import Data.Version
import Language.Preprocessor.Cpphs

main = defaultMain $
  simpleTool
    "myTool"
    (Version [3,1,4] [])
    (return Nothing)
    (\dir exts opts pkgdbs pkgs args -> print (dir, exts, opts, pkgdbs, pkgs, args))
    []

deriving instance Show CpphsOptions
deriving instance Show BoolOptions
