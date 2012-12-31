module Distribution.HaskellSuite.Tool
  (
    -- * Tool description
    PackageDbLoc
  , CompileFn
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
import Language.Preprocessor.Cpphs (CpphsOptions)
import Language.Haskell.Exts.Extension

type PackageDbLoc = FilePath

-- | Compilation function
type CompileFn = FilePath -> [Extension] -> CpphsOptions -> PackageDBStack -> [InstalledPackageId] -> [FilePath] -> IO ()

class Tool tool where
  toolName :: tool -> String
  toolVersion :: tool -> Version
  toolExtensions :: tool -> [String] -- ^ extensions of produced files
  toolGlobalDBLoc :: tool -> IO (Maybe PackageDbLoc)
  toolCompile :: tool -> CompileFn

  -- Methods that have default implementations

  toolLocateDB :: tool -> PackageDB -> IO (Maybe PackageDbLoc)
  toolLocateDB t GlobalPackageDB = toolGlobalDBLoc t
  toolLocateDB t UserPackageDB = Just <$> toolUserDBLoc t
  toolLocateDB _ (SpecificPackageDB p) = return $ Just p

  toolUserDBLoc :: tool -> IO PackageDbLoc
  toolUserDBLoc t =
    (</>) <$> haskellPackagesDir <*> pure (toolName t <.> "db")

  toolGetInstalledPkgs :: tool -> PackageDB -> IO Packages
  toolGetInstalledPkgs t db =
    withDBLoc t db $ maybe (return []) $ toolReadPackageDB t

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

  toolReadPackageDB :: tool -> PackageDbLoc -> IO Packages
  toolReadPackageDB _ = readDB

  toolWritePackageDB :: tool -> PackageDbLoc -> Packages -> IO ()
  toolWritePackageDB _ = writeDB

  toolRegister :: tool -> PackageDB -> InstalledPackageInfo -> IO ()
  toolRegister t db pkg = withDBLoc t db $ \mbdbloc ->
    case mbdbloc of
      Nothing -> throwIO RegisterNullDB
      Just dbloc -> do
        pkgs <- toolReadPackageDB t dbloc
        let pkgid = installedPackageId pkg
        when (isJust $ findPackage pkgid pkgs) $
          throwIO $ PkgExists pkgid
        toolWritePackageDB t dbloc $ pkg:pkgs

withDBLoc
  :: Tool tool
  => tool
  -> PackageDB
  -> (Maybe PackageDbLoc -> IO a)
  -> IO a
withDBLoc t db f = toolLocateDB t db >>= f

haskellPackagesDir :: IO FilePath
haskellPackagesDir = getAppUserDataDirectory "haskell-packages"

findPackage :: InstalledPackageId -> Packages -> Maybe InstalledPackageInfo
findPackage pkgid = find ((pkgid ==) . installedPackageId)

data SimpleTool = SimpleTool
  { stName :: String
  , stVer :: Version
  , stGlobalDBLoc :: IO (Maybe PackageDbLoc)
  , stCompile :: CompileFn
  , stExts :: [String]
  }

simpleTool
  :: String -- ^ tool name
  -> Version -- ^ tool version
  -> IO (Maybe PackageDbLoc) -- ^ location of global package database
  -> CompileFn
  -> [String] -- ^ extensions that generated file have
  -> SimpleTool
simpleTool = SimpleTool

instance Tool SimpleTool where
  toolName = stName
  toolVersion = stVer
  toolExtensions = stExts
  toolGlobalDBLoc = stGlobalDBLoc
  toolCompile = stCompile
