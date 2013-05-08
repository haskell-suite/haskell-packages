{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies,
             FlexibleInstances, TypeSynonymInstances,
             DeriveDataTypeable #-}
module Distribution.HaskellSuite.Helpers
  ( readPackagesInfo
  -- * Module monads
  , ModuleT
  , runModuleT
  , evalModuleT
  , MonadModule(..)
  , getModuleInfo
  , ModName(..)
  , convertModuleName
  -- * Useful re-exports
  , findModuleFile
  ) where

import Distribution.HaskellSuite.PackageDB
import Distribution.Simple.Compiler
import Distribution.Simple.Utils
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Text
import Distribution.ModuleName
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception
import Data.List
import Data.Typeable
import Data.Proxy
import qualified Data.Map as Map
import Text.Printf

data PkgInfoError = PkgInfoNotFound InstalledPackageId
  deriving Typeable
instance Exception PkgInfoError
instance Show PkgInfoError where
  show (PkgInfoNotFound pkgid) =
    printf "%s: package not found: %s" errPrefix (display pkgid)

readPackagesInfo
  :: IsPackageDB db
  => MaybeInitDB -> Proxy db -> [PackageDB] -> [InstalledPackageId] -> IO Packages
readPackagesInfo initDb proxyDb dbs pkgIds = do
  allPkgInfos <- concat <$> mapM (getInstalledPackages initDb proxyDb) dbs
  let
    pkgMap =
      Map.fromList
        [ (installedPackageId pkgInfo, pkgInfo)
        | pkgInfo <- allPkgInfos
        ]
  forM pkgIds $ \pkgId ->
    maybe
      (throwIO $ PkgInfoNotFound pkgId)
      return
      (Map.lookup pkgId pkgMap)
  -- return $ filter ((`Set.member` idSet) . installedPackageId) allPkgInfos

-- MonadModule class

class Monad m => MonadModule m where
  type ModuleInfo m
  lookupInCache :: ModName n => n -> m (Maybe (ModuleInfo m))
  insertInCache :: ModName n => n -> ModuleInfo m -> m ()
  getPackages :: m Packages
  readModuleInfo :: ModName n => [FilePath] -> n -> m (ModuleInfo m)

-- | Different libraries (Cabal, haskell-src-exts, ...) use different types
-- to represent module names. Hence this class.
class ModName n where
  modToString :: n -> String

instance ModName String where
  modToString = id

instance ModName ModuleName where
  modToString = display

convertModuleName :: (ModName n) => n -> ModuleName
convertModuleName = fromString . modToString

getModuleInfo :: (MonadModule m, ModName n) => n -> m (Maybe (ModuleInfo m))
getModuleInfo name = do
  cached <- lookupInCache name
  case cached of
    Just i -> return $ Just i
    Nothing -> do
      pkgs <- getPackages
      case findModule'sPackage pkgs name of
        Nothing -> return Nothing
        Just pkg -> do
          i <- readModuleInfo (libraryDirs pkg) name
          insertInCache name i
          return $ Just i

findModule'sPackage :: ModName n => Packages -> n -> Maybe InstalledPackageInfo
findModule'sPackage pkgs m = find ((convertModuleName m `elem`) . exposedModules) pkgs

-- ModuleT monad transformer

newtype ModuleT i m a =
  ModuleT
    (StateT (Map.Map ModuleName i)
      (ReaderT (Packages, [FilePath] -> ModuleName -> m i) m)
      a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans (ModuleT i) where
  lift = ModuleT . lift . lift

instance MonadIO m => MonadIO (ModuleT i m) where
  liftIO = ModuleT . liftIO

instance (Functor m, Monad m) => MonadModule (ModuleT i m) where
  type ModuleInfo (ModuleT i m) = i
  lookupInCache n = ModuleT $ Map.lookup (convertModuleName n) <$> get
  insertInCache n i = ModuleT $ modify $ Map.insert (convertModuleName n) i
  getPackages = ModuleT $ asks fst
  readModuleInfo dirs mod =
    lift =<< ModuleT (asks snd) <*> pure dirs <*> pure (convertModuleName mod)

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
