import Distribution.HaskellSuite.Cabal
import Distribution.HaskellSuite.PackageDB
import Data.Version

main = defaultMain testTool
  where
  testTool = HSTool
    { toolName = "myTool"
    , toolVersion = Version [3,1,4] []
    , toolGetInstalledPkgs = \_ -> return []
    , toolCompile = \dir args -> print (dir, args)
    , toolInstallLib = \a b c d e -> print (a,b,c,d,e)
    , toolRegister = register . locateDB (error "GlobalDB") (error "LocalDB")
    }
