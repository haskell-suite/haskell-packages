{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TemplateHaskell, ScopedTypeVariables #-}
module Distribution.HaskellSuite.PackageDB
  ( Packages
  , IsPackageDB(..)
  , IsDBName(..)
  , StandardDB(..)
  , getInstalledPackages
  , readPackagesInfo
  , writeDB
  , readDB
  , MaybeInitDB(..)
  , PkgDBError(..)
  , PkgInfoError(..)
  , errPrefix
  )
  where

import Data.Aeson
import Data.Aeson.TH
import Control.Applicative
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Exception as E
import Control.Monad
import Data.Typeable
import Data.Tagged
import Data.Proxy
import qualified Data.Map as Map
import Text.Printf
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Text
import System.FilePath
import System.Directory

-- The following imports are needed only for generation of JSON instances
import Data.Version (Version(..))
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.License (License(..))
import Distribution.ModuleName(ModuleName(..))

deriveJSON id ''License
deriveJSON id ''Version
deriveJSON id ''ModuleName
deriveJSON id ''PackageName
deriveJSON id ''PackageIdentifier
deriveJSON id ''InstalledPackageId
deriveJSON id ''InstalledPackageInfo_

type Packages = [InstalledPackageInfo]

data PkgDBError
  = BadPkgDB FilePath
  | PkgDBReadError FilePath IOException
  | PkgExists InstalledPackageId
  | RegisterNullDB
  deriving (Typeable)
errPrefix = "haskell-suite package manager"
instance Show PkgDBError where
  show (BadPkgDB path) =
    printf "%s: bad package database at %s" errPrefix path
  show (PkgDBReadError path e) =
    printf "%s: package db at %s could not be read: %s"
      errPrefix path (show e)
  show (PkgExists pkgid) =
    printf "%s: package %s is already in the database" errPrefix (display pkgid)
  show (RegisterNullDB) =
    printf "%s: attempt to register in a null global db" errPrefix
instance Exception PkgDBError

data MaybeInitDB = InitDB | Don'tInitDB

writeDB :: FilePath -> Packages -> IO ()
writeDB path db = LBS.writeFile path $ encode db

readDB :: MaybeInitDB -> FilePath -> IO Packages
readDB maybeInit path = do
  maybeDoInitDB

  cts <- LBS.fromChunks . return <$> BS.readFile path
    `E.catch` \e ->
      throwIO $ PkgDBReadError path e
  maybe (throwIO $ BadPkgDB path) return $ decode' cts

  where
    maybeDoInitDB
      | InitDB <- maybeInit = do
          dbExists <- doesFileExist path

          unless dbExists $ do
            writeDB path []

      | otherwise = return ()

class IsPackageDB db where

  -- | The name of the database. Used to construct some paths.
  dbName :: Tagged db String

  -- | Read a package database.
  --
  -- If the database does not exist, then the first argument tells whether
  -- we should create and initialize it with an empty package list. In
  -- that case, if 'Don'tInitDB' is specified, a 'BadPkgDb' exception is
  -- thrown.
  readPackageDB :: MaybeInitDB -> db -> IO Packages

  -- | Write a package database
  writePackageDB :: db -> Packages -> IO ()

  -- | Get the location of a global package database (if there's one)
  globalDB :: IO (Maybe db)

  -- | Create a db object given a database file path
  dbFromPath :: FilePath -> IO db

  -- Methods that have default implementations

  -- | Convert a package db specification to a db object
  locateDB :: PackageDB -> IO (Maybe db)
  locateDB GlobalPackageDB = globalDB
  locateDB UserPackageDB = Just <$> userDB
  locateDB (SpecificPackageDB p) = Just <$> dbFromPath p

  -- | The user database
  userDB :: IO db
  userDB = do
    let name = untag (dbName :: Tagged db String)
    path <- (</>) <$> haskellPackagesDir <*> pure (name <.> "db")
    dbFromPath path

haskellPackagesDir :: IO FilePath
haskellPackagesDir = getAppUserDataDirectory "haskell-packages"

class IsDBName name where
  getDBName :: Tagged name String

data StandardDB name = StandardDB FilePath

instance IsDBName name => IsPackageDB (StandardDB name) where
  dbName = retag (getDBName :: Tagged name String)

  readPackageDB init (StandardDB db) = readDB init db
  writePackageDB (StandardDB db) = writeDB db
  globalDB = return Nothing
  dbFromPath path = return $ StandardDB path

getInstalledPackages
  :: forall db. IsPackageDB db
  => MaybeInitDB
  -> Proxy db
  -> PackageDB
  -> IO Packages
getInstalledPackages initDb _proxy dbspec = do
  mbDb <- locateDB dbspec

  maybe
    (return [])
    (readPackageDB initDb)
    (mbDb :: Maybe db)

-- | Try to retrieve an 'InstalledPackageInfo' for each of
-- 'InstalledPackageId's from a specified set of 'PackageDB's.
--
-- May throw a 'PkgInfoNotFound' exception.
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

data PkgInfoError = PkgInfoNotFound InstalledPackageId
  deriving Typeable
instance Exception PkgInfoError
instance Show PkgInfoError where
  show (PkgInfoNotFound pkgid) =
    printf "%s: package not found: %s" errPrefix (display pkgid)
