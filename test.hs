import Distribution.HaskellSuite.Cabal
import Data.Version
import Distribution.InstalledPackageInfo

main = defaultMain testTool
  where
  testTool = HSTool
    { toolName = "myTool"
    , toolVersion = Version [3,1,4] []
    , toolGetInstalledPkgs = \_ -> return [emptyInstalledPackageInfo, emptyInstalledPackageInfo]
    }


