module Distribution.HaskellSuite.Simple where

import Distribution.HaskellSuite.Cabal
import Distribution.HaskellSuite.PackageDB
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Package
import Distribution.ModuleName
import Distribution.Simple.Compiler
import Distribution.InstalledPackageInfo
import System.FilePath
import System.Directory
import Control.Applicative
import Data.Version

installLib
  :: [String] -- ^ extensions of produced files
  -> (FilePath -> FilePath -> Maybe FilePath -> PackageIdentifier -> [ModuleName] -> IO ()) -- ^ can be used as 'toolInstallLib'
installLib exts buildDir targetDir _dynlibTargetDir _pkg mods =
  findModuleFiles [buildDir] exts mods
    >>= installOrdinaryFiles normal targetDir

haskellPackagesDir :: IO FilePath
haskellPackagesDir = getAppUserDataDirectory "haskell-packages"

userPackageDB
  :: String -- ^ tool name
  -> IO PackageDbLoc
userPackageDB name =
  (</>) <$> haskellPackagesDir <*> pure (name <.> "db")

simpleTool
  :: String -- ^ tool name
  -> Version -- ^ tool version
  -> PackageDbLoc -- ^ location of global package database
  -> (FilePath -> [FilePath] -> IO ()) -- ^ compilation function
  -> [String] -- ^ extensions that generated file have
  -> IO HSTool
simpleTool name ver globalDB compile exts = do
  localDB <- userPackageDB name
  let locDB = locateDB globalDB localDB

  return
    HSTool
      { toolName = name
      , toolVersion = ver
      , toolGetInstalledPkgs = readDB . locDB
      , toolCompile = compile
      , toolInstallLib = installLib exts
      , toolRegister = register . locDB
      }
