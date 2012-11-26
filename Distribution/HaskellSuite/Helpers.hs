module Distribution.HaskellSuite.Helpers where

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
import qualified Data.Set as Set
import Distribution.HaskellSuite.Tool

readPackagesInfo
  :: Tool tool
  => tool -> [PackageDB] -> [InstalledPackageId] -> IO [InstalledPackageInfo]
readPackagesInfo t dbs pkgIds = do
  let idSet = Set.fromList pkgIds
  allPkgInfos <- concat <$> mapM (toolGetInstalledPkgs t) dbs
  return $ filter ((`Set.member` idSet) . installedPackageId) allPkgInfos
