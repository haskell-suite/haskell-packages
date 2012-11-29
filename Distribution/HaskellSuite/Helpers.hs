{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Distribution.HaskellSuite.Helpers
  ( readPackagesInfo
  -- * Module monads
  , ModuleT
  , runModuleT
  , evalModuleT
  , MonadModule(..)
  , getModuleInfo
  -- * Useful re-exports
  , findModuleFile
  , ModuleName
  , Text(..)
  , fromString
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

-- MonadModule class

class Monad m => MonadModule m where
  type ModuleInfo m
  getModuleCache :: m (Map.Map ModuleName (ModuleInfo m))
  putModuleCache :: Map.Map ModuleName (ModuleInfo m) -> m ()
  getPackages :: m Packages
  readModuleInfo :: [FilePath] -> ModuleName -> m (ModuleInfo m)

getModuleInfo :: MonadModule m => ModuleName -> m (Maybe (ModuleInfo m))
getModuleInfo name = do
  cache <- getModuleCache
  case Map.lookup name cache of
    Just i -> return $ Just i
    Nothing -> do
      pkgs <- getPackages
      case findModule'sPackage pkgs name of
        Nothing -> return Nothing
        Just pkg -> do
          i <- readModuleInfo (libraryDirs pkg) name
          putModuleCache $ Map.insert name i cache
          return $ Just i

findModule'sPackage :: Packages -> ModuleName -> Maybe InstalledPackageInfo
findModule'sPackage pkgs m = find ((m `elem`) . exposedModules) pkgs

-- ModuleT monad transformer

newtype ModuleT i m a =
  ModuleT
    (StateT (Map.Map ModuleName i)
      (ReaderT (Packages, [FilePath] -> ModuleName -> m i) m)
      a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans (ModuleT i) where
  lift = ModuleT . lift . lift

instance (Functor m, Monad m) => MonadModule (ModuleT i m) where
  type ModuleInfo (ModuleT i m) = i
  getModuleCache = ModuleT get
  putModuleCache = ModuleT . put
  getPackages = ModuleT $ asks fst
  readModuleInfo dirs mod =
    lift =<< ModuleT (asks snd) <*> pure dirs <*> pure mod

runModuleT
  :: ModuleT i m a
  -> Packages
  -> ([FilePath] -> ModuleName -> m i)
  -> Map.Map ModuleName i
  -> m (a, Map.Map ModuleName i)
runModuleT (ModuleT a) pkgs readInfo modMap =
  runReaderT (runStateT a modMap) (pkgs, readInfo)

evalModuleT
  :: Functor m
  => ModuleT i m a
  -> Packages
  -> ([FilePath] -> ModuleName -> m i)
  -> Map.Map ModuleName i
  -> m a
evalModuleT a pkgs readInfo modMap =
  fst <$> runModuleT a pkgs readInfo modMap
