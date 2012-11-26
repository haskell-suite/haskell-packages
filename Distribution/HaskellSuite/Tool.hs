module Distribution.HaskellSuite.Tool
  (
    -- * Tool description
    PackageDbLoc
  , Tool(..)

    -- * Simple tool
  , SimpleTool
  , simpleTool
  )
  where

import Data.Version
import Distribution.HaskellSuite.PackageDB
import Distribution.Simple.Compiler
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.InstalledPackageInfo
import Distribution.ParseUtils
import Distribution.Package
import Distribution.Text
import Distribution.ModuleName
import System.FilePath
import System.Directory
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Maybe
import Data.List

type PackageDbLoc = FilePath

class Tool tool where
  toolName :: tool -> String
  toolVersion :: tool -> Version
  toolExtensions :: tool -> [String] -- ^ extensions of produced files
  toolGlobalDBLoc :: tool -> IO PackageDbLoc
  toolCompile :: tool -> FilePath -> [InstalledPackageId] -> [FilePath] -> IO ()

  -- Methods that have default implementations

  toolLocateDB :: tool -> PackageDB -> IO PackageDbLoc
  toolLocateDB t GlobalPackageDB = toolGlobalDBLoc t
  toolLocateDB t UserPackageDB = toolUserDBLoc t
  toolLocateDB _ (SpecificPackageDB p) = return p

  toolUserDBLoc :: tool -> IO PackageDbLoc
  toolUserDBLoc t =
    (</>) <$> haskellPackagesDir <*> pure (toolName t <.> "db")

  toolGetInstalledPkgs :: tool -> PackageDB -> IO [InstalledPackageInfo]
  toolGetInstalledPkgs t db =
    withDBLoc t db $ toolReadPackageDB t

  toolInstallLib
      :: tool
      -> FilePath -- ^ build dir
      -> FilePath -- ^ target dir
      -> Maybe FilePath -- ^ target dir for dynamic libraries
      -> PackageIdentifier
      -> [ModuleName]
      -> IO ()
  toolInstallLib t buildDir targetDir _dynlibTargetDir _pkg mods =
    findModuleFiles [buildDir] (toolExtensions t) mods
      >>= installOrdinaryFiles normal targetDir

  toolReadPackageDB :: tool -> PackageDbLoc -> IO [InstalledPackageInfo]
  toolReadPackageDB _ = readDB

  toolWritePackageDB :: tool -> PackageDbLoc -> [InstalledPackageInfo] -> IO ()
  toolWritePackageDB _ = writeDB

  toolRegister :: tool -> PackageDB -> InstalledPackageInfo -> IO ()
  toolRegister t db pkg = withDBLoc t db $ \dbloc -> do
    pkgs <- toolReadPackageDB t dbloc
    let pkgid = installedPackageId pkg
    when (isJust $ findPackage pkgid pkgs) $
      throwIO $ PkgExists pkgid
    toolWritePackageDB t dbloc $ pkg:pkgs

withDBLoc
  :: Tool tool
  => tool
  -> PackageDB
  -> (PackageDbLoc -> IO a)
  -> IO a
withDBLoc t db f = toolLocateDB t db >>= f

haskellPackagesDir :: IO FilePath
haskellPackagesDir = getAppUserDataDirectory "haskell-packages"

findPackage :: InstalledPackageId -> Packages -> Maybe InstalledPackageInfo
findPackage pkgid = find ((pkgid ==) . installedPackageId)

data SimpleTool = SimpleTool
  { stName :: String
  , stVer :: Version
  , stGlobalDBLoc :: IO PackageDbLoc
  , stCompile :: (FilePath -> [InstalledPackageId] -> [FilePath] -> IO ())
  , stExts :: [String]
  }

simpleTool
  :: String -- ^ tool name
  -> Version -- ^ tool version
  -> IO PackageDbLoc -- ^ location of global package database
  -> (FilePath -> [InstalledPackageId] -> [FilePath] -> IO ()) -- ^ compilation function
  -> [String] -- ^ extensions that generated file have
  -> SimpleTool
simpleTool = SimpleTool

instance Tool SimpleTool where
  toolName = stName
  toolVersion = stVer
  toolExtensions = stExts
  toolGlobalDBLoc = stGlobalDBLoc
  toolCompile = stCompile
