{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
-- | This module is designed to be imported qualified:
--
-- >import qualified Distribution.HaskellSuite.Compiler as Compiler
module Distribution.HaskellSuite.Compiler
  (
    -- * Compiler description
    CompileFn
  , Is(..)

    -- * Simple compiler
  , Simple
  , simple
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
import Distribution.ModuleName (ModuleName)
import System.FilePath
import System.Directory
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Maybe
import Data.List
import Language.Haskell.Exts.Annotated.CPP
import Language.Haskell.Exts.Extension

-- | Compilation function
type CompileFn = FilePath -> [Extension] -> CpphsOptions -> PackageDBStack -> [InstalledPackageId] -> [FilePath] -> IO ()

class IsPackageDB (DB compiler) => Is compiler where

  type DB compiler

  name :: compiler -> String
  version :: compiler -> Version
  fileExtensions :: compiler -> [String] -- ^ extensions of produced files
  compile :: compiler -> CompileFn
  languageExtensions :: compiler -> [Extension]

  installLib
      :: compiler
      -> FilePath -- ^ build dir
      -> FilePath -- ^ target dir
      -> Maybe FilePath -- ^ target dir for dynamic libraries
      -> PackageIdentifier
      -> [ModuleName]
      -> IO ()
  installLib t buildDir targetDir _dynlibTargetDir _pkg mods =
    findModuleFiles [buildDir] (fileExtensions t) mods
      >>= installOrdinaryFiles normal targetDir

  register
    :: compiler
    -> PackageDB
    -> InstalledPackageInfo
    -> IO ()
  register t dbspec pkg = do
    mbDb <- locateDB dbspec

    case mbDb :: Maybe (DB compiler) of
      Nothing -> throwIO RegisterNullDB
      Just db -> do
        pkgs <- readPackageDB InitDB db
        let pkgid = installedPackageId pkg
        when (isJust $ findPackage pkgid pkgs) $
          throwIO $ PkgExists pkgid
        writePackageDB db $ pkg:pkgs

findPackage :: InstalledPackageId -> Packages -> Maybe InstalledPackageInfo
findPackage pkgid = find ((pkgid ==) . installedPackageId)

data Simple db = Simple
  { stName :: String
  , stVer :: Version
  , stLangExts :: [Extension]
  , stCompile :: CompileFn
  , stExts :: [String]
  }

simple
  :: String -- ^ compiler name
  -> Version -- ^ compiler version
  -> [Extension]
  -> CompileFn
  -> [String] -- ^ extensions that generated file have
  -> Simple db
simple = Simple

instance IsPackageDB db => Is (Simple db) where
  type DB (Simple db) = db

  name = stName
  version = stVer
  fileExtensions = stExts
  compile = stCompile
  languageExtensions = stLangExts
