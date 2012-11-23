module Distribution.HaskellSuite.Helpers where

import Distribution.HaskellSuite.Cabal
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Package
import Distribution.ModuleName

installLib
  :: [String] -- ^ extensions of produced files
  -> (FilePath -> FilePath -> Maybe FilePath -> PackageIdentifier -> [ModuleName] -> IO ()) -- ^ function that can be used as 'toolInstallLib'
installLib exts buildDir targetDir _dynlibTargetDir _pkg mods =
  findModuleFiles [buildDir] exts mods
    >>= installOrdinaryFiles normal targetDir
