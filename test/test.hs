import Distribution.HaskellSuite.Cabal
import Distribution.HaskellSuite.PackageDB
import Data.Version

main = defaultMain testTool
  where
  ldb = locateDB (error "GlobalDB") (error "LocalDB")

  testTool = HSTool
    { toolName = "myTool"
    , toolVersion = Version [3,1,4] []
    , toolGetInstalledPkgs = readDB . ldb
    , toolCompile = \dir pkgs args -> print (dir, pkgs, args)
    , toolInstallLib = \a b c d e -> print (a,b,c,d,e)
    , toolRegister = register . ldb
    }
