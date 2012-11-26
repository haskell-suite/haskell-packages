{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TemplateHaskell #-}
module Distribution.HaskellSuite.PackageDB where

import Data.Aeson
import Data.Aeson.TH
import Control.Applicative
import qualified Data.ByteString.Lazy as BS
import Control.Exception
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
  deriving (Typeable)
eprefix = "haskell-suite package manager"
instance Show PkgDBError where
  show (BadPkgDB path) =
    printf "%s: bad package database at %s" eprefix path
  show (PkgDBReadError path e) =
    printf "%s: package db at %s could not be read: %s"
      eprefix path (show e)
  show (PkgExists pkgid) =
    printf "%s: package %s is already in the database" eprefix (display pkgid)
instance Exception PkgDBError

writeDB :: FilePath -> Packages -> IO ()
writeDB path db = BS.writeFile path $ encode db

readDB :: FilePath -> IO Packages
readDB path = do
  cts <- evaluate . force =<< BS.readFile path
    `catch` \e ->
      throwIO $ PkgDBReadError path e
  maybe (throwIO $ BadPkgDB path) return $ decode' cts
