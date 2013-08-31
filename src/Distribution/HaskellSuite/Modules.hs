{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies,
             FlexibleInstances, TypeSynonymInstances,
             DeriveDataTypeable, StandaloneDeriving,
             MultiParamTypeClasses, UndecidableInstances,
             ScopedTypeVariables #-}
module Distribution.HaskellSuite.Modules
  (
  -- * Module monad
  -- | When you need to resolve modules, you work in a 'ModuleT' monad (or
  -- another monad that is an instance of 'MonadModule') and use the
  -- 'getModuleInfo' function.
  --
  -- It finds an installed module by its name and reads (and caches) its
  -- info from the info file. Then you run a 'ModuleT' monadic action
  -- using 'evalModuleT' or 'runModuleT'.
  --
  -- To run a 'ModuleT' action you'll also need to provide the set of
  -- packages (represented by their 'InstalledPackageInfo') in which to
  -- search for modules. You can get such a set from either
  -- 'getInstalledPackages' or 'readPackagesInfo', depending on your use
  -- case.
    ModuleT
  , getModuleInfo
  , evalModuleT
  , runModuleT
  , MonadModule(..)
  -- * Module names
  , ModName(..)
  , convertModuleName
  ) where
import Distribution.HaskellSuite.Packages
import Distribution.Simple.Utils
import Distribution.InstalledPackageInfo
import Distribution.Text
import Distribution.ModuleName
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import qualified Data.Map as Map
import System.FilePath

-- | This class defines the interface that is used by 'getModuleInfo', so
-- that you can use it in monads other than 'ModuleT'.
--
-- You don't typically have to define your own instances of this class, but
-- here are a couple of cases when you might:
--
-- * A pure (non-'MonadIO') mockup module monad for testing purposes
--
-- * A transformer over 'ModuleT'
--
-- * You need a more complex way to retrieve the module info
class Monad m => MonadModule m where
  -- | The type of module info
  type ModuleInfo m
  lookupInCache :: ModName n => n -> m (Maybe (ModuleInfo m))
  insertInCache :: ModName n => n -> ModuleInfo m -> m ()
  getPackages :: m Packages

  -- | Read the module info, given a list of search paths and the module
  -- name
  readModuleInfo :: ModName n => [FilePath] -> n -> m (ModuleInfo m)

-- | Different libraries (Cabal, haskell-src-exts, ...) use different types
-- to represent module names. Hence this class.
class ModName n where
  modToString :: n -> String

instance ModName String where
  modToString = id

instance ModName ModuleName where
  modToString = display

-- | Convert module name from arbitrary representation to Cabal's one
convertModuleName :: (ModName n) => n -> ModuleName
convertModuleName = fromString . modToString

-- | Tries to find the module in the current set of packages, then find the
-- module's info file, and reads and caches its contents.
--
-- Returns 'Nothing' if the module could not be found in the current set of
-- packages. If the module is found, but something else goes wrong (e.g.
-- there's no info file for it), an exception is thrown.
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

-- | A standard module monad transformer.
--
-- @i@ is the type of module info, @m@ is the underlying monad.
newtype ModuleT i m a =
  ModuleT { unModuleT ::
    (StateT (Map.Map ModuleName i)
      (ReaderT (Packages, [FilePath] -> ModuleName -> m i) m)
      a)
  }
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

mapModuleT :: Monad m => (m a -> m b) -> ModuleT i m a -> ModuleT i m b
mapModuleT f m = ModuleT $ mapStateT (mapReaderT f') (unModuleT m)
  where
    f' ma = do
      (a,c) <- ma
      b <- f (return a)
      return (b,c)

instance MonadReader r m => MonadReader r (ModuleT i m) where
  ask    = lift ask
  local  = mapModuleT . local
  reader = lift . reader

instance MonadState s m => MonadState s (ModuleT i m) where
  get   = lift get
  put   = lift . put
  state = lift . state

deriving instance MonadWriter w m => MonadWriter w (ModuleT i m)

deriving instance MonadError e m => MonadError e (ModuleT i m)

deriving instance MonadCont m => MonadCont (ModuleT i m)

-- | Run a 'ModuleT' action
runModuleT
  :: MonadIO m
  => ModuleT i m a -- ^ the monadic action to run
  -> Packages -- ^ packages in which to look for modules
  -> String -- ^ file extension of info files
  -> (FilePath -> m i) -- ^ how to read information from an info file
  -> Map.Map ModuleName i -- ^ initial set of module infos
  -> m (a, Map.Map ModuleName i)
  -- ^ return value, plus all cached module infos (that is, the initial set
  -- plus all infos that have been read by the action itself)
runModuleT (ModuleT a) pkgs suffix readInfo modMap =
  runReaderT (runStateT a modMap) (pkgs, findAndReadInfo)
  where
    findAndReadInfo dirs name = do
      (base, rel) <- liftIO $ findModuleFile dirs [suffix] name
      readInfo $ base </> rel

-- | Run a 'ModuleT' action.
--
-- This is a simplified version of 'runModuleT'.
evalModuleT
  :: MonadIO m
  => ModuleT i m a -- ^ the monadic action to run
  -> Packages -- ^ packages in which to look for modules
  -> String -- ^ file extension of info files
  -> (FilePath -> m i) -- ^ how to read information from an info file
  -> m a
evalModuleT a pkgs suffix readInfo =
  fst `liftM` runModuleT a pkgs suffix readInfo Map.empty
