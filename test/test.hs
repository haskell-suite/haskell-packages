{-# LANGUAGE StandaloneDeriving #-}
import Distribution.HaskellSuite.Cabal
import Distribution.HaskellSuite.Tool
import Data.Version
import Language.Haskell.Exts.Annotated.CPP

main = defaultMain $
  simpleTool
    "myTool"
    (Version [3,1,4] [])
    []
    (return Nothing)
    (\dir exts opts pkgdbs pkgs args -> print (dir, exts, opts, pkgdbs, pkgs, args))
    []

deriving instance Show CpphsOptions
deriving instance Show BoolOptions
