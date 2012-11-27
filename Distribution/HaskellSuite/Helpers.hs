{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.HaskellSuite.Helpers
  ( readPackagesInfo
  , ModuleT
  , runModuleT
  , getModuleInfo
  ) where

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
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception
import Data.Maybe
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Distribution.HaskellSuite.Tool

readPackagesInfo
  :: Tool tool
  => tool -> [PackageDB] -> [InstalledPackageId] -> IO Packages
readPackagesInfo t dbs pkgIds = do
  let idSet = Set.fromList pkgIds
  allPkgInfos <- concat <$> mapM (toolGetInstalledPkgs t) dbs
  return $ filter ((`Set.member` idSet) . installedPackageId) allPkgInfos

findModule'sPackage :: Packages -> ModuleName -> Maybe InstalledPackageInfo
findModule'sPackage pkgs m = find ((m `elem`) . exposedModules) pkgs

newtype ModuleT i m a =
  ModuleT
    (StateT (Map.Map ModuleName i)
      (ReaderT (Packages, [FilePath] -> ModuleName -> m i) m)
      a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans (ModuleT i) where
  lift = ModuleT . lift . lift

runModuleT
  :: ModuleT i m a
  -> Packages
  -> ([FilePath] -> ModuleName -> m i)
  -> Map.Map ModuleName i
  -> m (a, Map.Map ModuleName i)
runModuleT (ModuleT a) pkgs readInfo modMap =
  runReaderT (runStateT a modMap) (pkgs, readInfo)

getModuleInfo :: Monad m => ModuleName -> ModuleT i m (Maybe i)
getModuleInfo name = ModuleT $ do
  modMap <- get
  case Map.lookup name modMap of
    Just i -> return $ Just i
    Nothing -> do
      (pkgs, readInfo) <- ask
      case findModule'sPackage pkgs name of
        Nothing -> return Nothing
        Just pkg -> do
          i <- lift $ lift $ readInfo (libraryDirs pkg) name
          put $ Map.insert name i modMap
          return $ Just i
