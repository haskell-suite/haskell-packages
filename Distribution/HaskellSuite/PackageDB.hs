{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TemplateHaskell #-}
module Distribution.HaskellSuite.PackageDB
  ( Packages
  , writeDB
  , readDB
  , MaybeInitDB(..)
  , PkgDBError(..)
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
import Control.DeepSeq
import Data.Typeable
import Data.Monoid
import Data.Maybe
import Data.List
import Text.Printf
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Text
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

-- | Read a package database.
--
-- If the database does not exist, then the first argument tells whether we
-- should create and initialize it with an empty package list. In that
-- case, if 'Don'tInitDB' is specified, a 'BadPkgDb' exception is thrown.
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
