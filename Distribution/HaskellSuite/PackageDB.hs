{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Distribution.HaskellSuite.PackageDB
  where

import Data.Aeson
import Data.Aeson.TH
import Control.Applicative
import Data.ByteString.Lazy as BS
import Control.Exception
import Data.Typeable
import Data.Monoid
import Text.Printf
import Distribution.InstalledPackageInfo

deriveJSON id ''InstalledPackageInfo

type Packages = [InstalledPackageInfo]

data PkgDBError
  = BadPkgDB FilePath
  | PkgDBReadError FilePath IOException
  deriving (Typeable)
eprefix = "haskell-suite package manager"
instance Show PkgDBError where
  show (BadPkgDB path) =
    printf "%s: bad package database at %s" eprefix path
  show (PkgDBReadError path e) =
    printf "%s: package db at %s could not be read: %s"
      eprefix path (show e)
instance Exception PkgDBError

writeDB :: FilePath -> Packages -> IO ()
writeDB path db = BS.writeFile path $ encode db

readDB :: FilePath -> IO Packages
readDB path = do
  cts <- BS.readFile path
    `catch` \e ->
      throwIO $ PkgDBReadError path e
  maybe (throwIO $ BadPkgDB path) return $ decode cts
