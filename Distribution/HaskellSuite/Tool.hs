{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
module Distribution.HaskellSuite.Tool
  (
    -- * Tool description
    CompileFn
  , IsCompiler(..)

    -- * Simple compiler
  , SimpleCompiler
  , simpleCompiler
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
import Language.Haskell.Exts.Annotated.CPP
import Language.Haskell.Exts.Extension

-- | Compilation function
type CompileFn = FilePath -> [Extension] -> CpphsOptions -> PackageDBStack -> [InstalledPackageId] -> [FilePath] -> IO ()

class
  IsPackageDB (CompilerDB compiler) =>
  IsCompiler compiler where

  type CompilerDB compiler

  toolName :: compiler -> String
  toolVersion :: compiler -> Version
  toolExtensions :: compiler -> [String] -- ^ extensions of produced files
  toolCompile :: compiler -> CompileFn
  toolLanguageExtensions :: compiler -> [Extension]

  toolInstallLib
      :: compiler
      -> FilePath -- ^ build dir
      -> FilePath -- ^ target dir
      -> Maybe FilePath -- ^ target dir for dynamic libraries
      -> PackageIdentifier
      -> [ModuleName]
      -> IO ()
  toolInstallLib t buildDir targetDir _dynlibTargetDir _pkg mods =
    findModuleFiles [buildDir] (toolExtensions t) mods
      >>= installOrdinaryFiles normal targetDir

  toolRegister
    :: compiler
    -> PackageDB
    -> InstalledPackageInfo
    -> IO ()
  toolRegister t dbspec pkg = do
    mbDb <- locateDB dbspec

    case mbDb :: Maybe (CompilerDB compiler) of
      Nothing -> throwIO RegisterNullDB
      Just db -> do
        pkgs <- readPackageDB InitDB db
        let pkgid = installedPackageId pkg
        when (isJust $ findPackage pkgid pkgs) $
          throwIO $ PkgExists pkgid
        writePackageDB db $ pkg:pkgs

findPackage :: InstalledPackageId -> Packages -> Maybe InstalledPackageInfo
findPackage pkgid = find ((pkgid ==) . installedPackageId)

data SimpleCompiler db = SimpleCompiler
  { stName :: String
  , stVer :: Version
  , stLangExts :: [Extension]
  , stCompile :: CompileFn
  , stExts :: [String]
  }

simpleCompiler
  :: String -- ^ tool name
  -> Version -- ^ tool version
  -> [Extension]
  -> CompileFn
  -> [String] -- ^ extensions that generated file have
  -> SimpleCompiler db
simpleCompiler = SimpleCompiler

instance IsPackageDB db => IsCompiler (SimpleCompiler db) where
  type CompilerDB (SimpleCompiler db) = db

  toolName = stName
  toolVersion = stVer
  toolExtensions = stExts
  toolCompile = stCompile
  toolLanguageExtensions = stLangExts
