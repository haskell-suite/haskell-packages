import Distribution.HaskellSuite.Cabal
import Distribution.HaskellSuite.Tool
import Data.Version

main = defaultMain $
  simpleTool
    "myTool"
    (Version [3,1,4] [])
    (return Nothing)
    (\dir pkgdbs pkgs args -> print (dir, pkgdbs, pkgs, args))
    []
