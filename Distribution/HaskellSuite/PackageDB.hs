{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Distribution.HaskellSuite.PackageDB
  ( PackageDB
  , writeDB
  , readDB
  , PkgDBError(..)
  , toPackageList
  , fromPackageList
  )
  where

import Distribution.HaskellSuite.PackageInfo
import Data.Aeson
import Control.Applicative
import Data.ByteString.Lazy as BS
import Control.Exception
import Data.Typeable
import Data.Monoid
import Text.Printf

newtype PackageDB = PackageDB [PackageInfo]
  deriving (FromJSON, ToJSON, Monoid)

toPackageList :: PackageDB -> [PackageInfo]
toPackageList (PackageDB l) = l

fromPackageList :: [PackageInfo] -> PackageDB
fromPackageList = PackageDB

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

writeDB :: FilePath -> PackageDB -> IO ()
writeDB path db = BS.writeFile path $ encode db

readDB :: FilePath -> IO PackageDB
readDB path = do
  cts <- BS.readFile path
    `catch` \e ->
      throwIO $ PkgDBReadError path e
  maybe (throwIO $ BadPkgDB path) return $ decode cts
