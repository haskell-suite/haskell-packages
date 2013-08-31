{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TemplateHaskell, ScopedTypeVariables, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.HaskellSuite.Packages
  (
  -- * Querying package databases
  -- | 'getInstalledPackages' and 'readPackagesInfo' can be used to get
  -- package information from package databases.
  --
  -- They use the 'IsPackageDB' interface, so that you can use them with
  -- your own, custom databases.
  --
  -- Use 'getInstalledPackages' to get all packages defined in a particular
  -- database, and 'readPackagesInfo' when you're searching for
  -- a particular set of packages in a set of databases.
    Packages
  , getInstalledPackages
  , readPackagesInfo
  -- * IsPackageDB class and friends
  , IsPackageDB(..)
  , MaybeInitDB(..)
  , maybeInitDB
  -- * StandardDB
  -- | 'StandardDB' is a simple `IsPackageDB` implementation which cover many
  -- (but not all) use cases. Please see the source code to see what
  -- assumptions it makes and whether they hold for your use case.
  , StandardDB(..)
  , IsDBName(..)

  -- * Relative paths in package databases
  -- | Traditionally, the paths in package databases are absolute.
  --
  -- haskell-packages allows relative file paths in databases, which is
  -- useful in some cases (e.g. relocatable global package database).
  --
  -- By default, 'readPackageDB' (for 'StandardDB') treats relative paths
  -- as being relative to the database path.
  --
  -- However, Cabal still passes absolute file names, and by default
  -- 'writePackageDB' stores them verbatim. To change this, use
  -- 'makePkgInfoRelative' in your implementation of 'writePackageDB'.
  , makePkgInfoRelative
  , makePkgInfoAbsolute
  , mapPaths

  -- * Direct database manipulation
  -- | 'writeDB' and 'readDB' perform (de)serialization of a package
  -- database using a simple JSON encoding. You may use these to implement
  -- 'writePackageDB' and 'readPackageDB' for your own databases.
  , writeDB
  , readDB
  , initDB
  -- * Exceptions
  , PkgDBError(..)
  , PkgInfoError(..)
  )
  where

import Data.Aeson
import Data.Aeson.Types
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
import qualified Distribution.InstalledPackageInfo as Info
import Distribution.Package
import Distribution.Text
import System.FilePath
import System.Directory

-- The following imports are needed only for JSON instances
import Data.Version (Version(..))
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.License (License(..))
import Distribution.ModuleName(ModuleName)
import Distribution.Simple.Utils
import Distribution.Verbosity

--------------
-- Querying --
--------------

type Packages = [Info.InstalledPackageInfo]

-- | Get all packages that are registered in a particular database
--
-- If the database doesn't exist, the behaviour is determined by
-- 'maybeInitDB'.
getInstalledPackages
  :: forall db. IsPackageDB db
  => Proxy db
  -> PackageDB
  -> IO Packages
getInstalledPackages _proxy dbspec = do
  mbDb <- locateDB dbspec

  maybe
    (return [])
    (readPackageDB $ maybeInitDB dbspec)
    (mbDb :: Maybe db)

-- | Try to retrieve an 'InstalledPackageInfo' for each of
-- 'InstalledPackageId's from a specified set of 'PackageDB's.
--
-- May throw a 'PkgInfoNotFound' exception.
--
-- If a database doesn't exist, the behaviour is determined by
-- 'maybeInitDB'.
readPackagesInfo
  :: IsPackageDB db
  => Proxy db -> [PackageDB] -> [InstalledPackageId] -> IO Packages
readPackagesInfo proxyDb dbs pkgIds = do
  allPkgInfos <- concat <$> mapM (getInstalledPackages proxyDb) dbs
  let
    pkgMap =
      Map.fromList
        [ (Info.installedPackageId pkgInfo, pkgInfo)
        | pkgInfo <- allPkgInfos
        ]
  forM pkgIds $ \pkgId ->
    maybe
      (throwIO $ PkgInfoNotFound pkgId)
      return
      (Map.lookup pkgId pkgMap)

---------------------------
-- IsPackageDB & friends --
---------------------------

-- | Package database class.
--
-- @db@ will typically be a newtype-wrapped path to the database file,
-- although more sophisticated setups are certainly possible.
  --
  -- Consider using 'StandardDB' first, and implement your own database
  -- type if that isn't enough.
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

-- | A flag which tells whether the library should create an empty package
-- database if it doesn't exist yet
data MaybeInitDB = InitDB | Don'tInitDB

-- | This function determines whether a package database should be
-- initialized if it doesn't exist yet.
--
-- The rule is this: if it is a global or a user database, then initialize
-- it; otherwise, don't.
--
-- Rationale: if the database was specified by the user, she could have
-- made a mistake in the path, and we'd rather report it. On the other
-- hand, it is our responsibility to ensure that the user and global
-- databases exist.
maybeInitDB :: PackageDB -> MaybeInitDB
maybeInitDB GlobalPackageDB = InitDB
maybeInitDB UserPackageDB   = InitDB
maybeInitDB SpecificPackageDB {} = Don'tInitDB

----------------
-- StandardDB --
----------------

class IsDBName name where
  getDBName :: Tagged name String

data StandardDB name = StandardDB FilePath

instance IsDBName name => IsPackageDB (StandardDB name) where
  dbName = retag (getDBName :: Tagged name String)

  readPackageDB init (StandardDB db) =
    map (makePkgInfoAbsolute (dropFileName db)) <$> readDB init db
  writePackageDB (StandardDB db) = writeDB db
  globalDB = return Nothing
  dbFromPath path = return $ StandardDB path

---------------------------------
-- Absolute and relative paths --
---------------------------------

-- | Make all paths in the package info relative to the given base
-- directory.
makePkgInfoRelative :: FilePath -> Info.InstalledPackageInfo -> Info.InstalledPackageInfo
makePkgInfoRelative base info =
  mapPaths (makeRelative base) info

-- | Make all relative paths in the package info absolute, interpreting
-- them relative to the given base directory.
makePkgInfoAbsolute :: FilePath -> Info.InstalledPackageInfo -> Info.InstalledPackageInfo
makePkgInfoAbsolute base info =
  flip mapPaths info $ \f ->
    if isRelative f
      then base </> f
      else f

-- | Apply a given function to all file paths contained in the package info
mapPaths
  :: (FilePath -> FilePath)
  -> (Info.InstalledPackageInfo -> Info.InstalledPackageInfo)
mapPaths f info = info
  { Info.importDirs = map f (Info.importDirs info)
  , Info.libraryDirs = map f (Info.libraryDirs info)
  , Info.includeDirs = map f (Info.includeDirs info)
  , Info.frameworkDirs = map f (Info.frameworkDirs info)
  , Info.haddockInterfaces = map f (Info.haddockInterfaces info)
  , Info.haddockHTMLs = map f (Info.haddockHTMLs info)
  }

-------------------------
-- Auxiliary functions --
-------------------------

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
      | InitDB <- maybeInit = initDB path
      | otherwise = return ()

-- | If the path does not exist, create an empty database there. Otherwise,
-- do nothing.
initDB :: FilePath -> IO ()
initDB path = do
  dbExists <- doesFileExist path
  unless dbExists $ do
    createDirectoryIfMissingVerbose silent True (dropFileName path)
    writeDB path []

haskellPackagesDir :: IO FilePath
haskellPackagesDir = getAppUserDataDirectory "haskell-packages"

----------------
-- Exceptions --
----------------

errPrefix :: String
errPrefix = "haskell-suite package manager"

data PkgDBError
  = BadPkgDB FilePath -- ^ package database could not be parsed or contains errors
  | PkgDBReadError FilePath IOException -- ^ package db file could not be read
  | PkgExists InstalledPackageId -- ^ attempt to register an already present package id
  | RegisterNullDB -- ^ attempt to register in the global db when it's not present
  deriving (Typeable)
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

data PkgInfoError
  = PkgInfoNotFound InstalledPackageId
  -- ^ requested package id could not be found in any of the package databases
  deriving Typeable
instance Exception PkgInfoError
instance Show PkgInfoError where
  show (PkgInfoNotFound pkgid) =
    printf "%s: package not found: %s" errPrefix (display pkgid)

---------------------
-- Aeson instances --
---------------------

stdToJSON :: Text a => a -> Value
stdToJSON = toJSON . display
stdFromJSON :: Text a => Value -> Parser a
stdFromJSON = maybe mzero return . simpleParse <=< parseJSON

instance ToJSON License where
  toJSON = stdToJSON
instance FromJSON License where
  parseJSON = stdFromJSON

instance ToJSON Version where
  toJSON = stdToJSON
instance FromJSON Version where
  parseJSON = stdFromJSON

instance ToJSON ModuleName where
  toJSON = stdToJSON
instance FromJSON ModuleName where
  parseJSON = stdFromJSON

instance ToJSON PackageName where
  toJSON = stdToJSON
instance FromJSON PackageName where
  parseJSON = stdFromJSON

instance ToJSON PackageIdentifier where
  toJSON = stdToJSON
instance FromJSON PackageIdentifier where
  parseJSON = stdFromJSON

instance ToJSON InstalledPackageId where
  toJSON = stdToJSON
instance FromJSON InstalledPackageId where
  parseJSON = stdFromJSON

instance ToJSON a => ToJSON (Info.InstalledPackageInfo_ a) where
  toJSON i = object
    [ "id" .= Info.installedPackageId i
    , "name" .= Info.sourcePackageId i
    , "license" .= Info.license i
    , "copyright" .= Info.copyright i
    , "maintainer" .= Info.maintainer i
    , "author" .= Info.author i
    , "stability" .= Info.stability i
    , "homepage" .= Info.homepage i
    , "package-url" .= Info.pkgUrl i
    , "synopsis" .= Info.synopsis i
    , "description" .= Info.description i
    , "category" .= Info.category i
    , "exposed" .= Info.exposed i
    , "exposed-modules" .= Info.exposedModules i
    , "hidden-modules" .= Info.hiddenModules i
    , "trusted" .= Info.trusted i
    , "import-dirs" .= Info.importDirs i
    , "library-dirs" .= Info.libraryDirs i
    , "hs-libraries" .= Info.hsLibraries i
    , "extra-libraries" .= Info.extraLibraries i
    , "extra-ghci-libraries" .= Info.extraGHCiLibraries i
    , "include-dirs" .= Info.includeDirs i
    , "includes" .= Info.includes i
    , "depends" .= Info.depends i
    , "hugs-options" .= Info.hugsOptions i
    , "cc-options" .= Info.ccOptions i
    , "ld-options" .= Info.ldOptions i
    , "framework-dirs" .= Info.frameworkDirs i
    , "frameworks" .= Info.frameworks i
    , "haddock-interfaces" .= Info.haddockInterfaces i
    , "haddock-html" .= Info.haddockHTMLs i
    ]

-- FIXME this will break silently if the order of fields changes in the
-- future
instance FromJSON a => FromJSON (Info.InstalledPackageInfo_ a) where
  parseJSON (Object v) = Info.InstalledPackageInfo <$>
    v .: "id" <*>
    v .: "name" <*>
    v .: "license" <*>
    v .: "copyright" <*>
    v .: "maintainer" <*>
    v .: "author" <*>
    v .: "stability" <*>
    v .: "homepage" <*>
    v .: "package-url" <*>
    v .: "synopsis" <*>
    v .: "description" <*>
    v .: "category" <*>
    v .: "exposed" <*>
    v .: "exposed-modules" <*>
    v .: "hidden-modules" <*>
    v .: "trusted" <*>
    v .: "import-dirs" <*>
    v .: "library-dirs" <*>
    v .: "hs-libraries" <*>
    v .: "extra-libraries" <*>
    v .: "extra-ghci-libraries" <*>
    v .: "include-dirs" <*>
    v .: "includes" <*>
    v .: "depends" <*>
    v .: "hugs-options" <*>
    v .: "cc-options" <*>
    v .: "ld-options" <*>
    v .: "framework-dirs" <*>
    v .: "frameworks" <*>
    v .: "haddock-interfaces" <*>
    v .: "haddock-html"
  parseJSON _ = mzero
