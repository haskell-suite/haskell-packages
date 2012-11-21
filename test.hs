import Distribution.HaskellSuite.Cabal
import Data.Version

main = defaultMain testTool
  where
  testTool = HSTool
    { toolName = "myTool"
    , toolVersion = Version [3,1,4] []
    , toolGetInstalledPkgs = \_ -> return []
    , toolCompile = \dir args -> print (dir, args)
    , toolRegister = \db _ -> print db
    }


