-- | This module provides Cabal integration.
{-# LANGUAGE RecordWildCards #-}

module Distribution.HaskellSuite.Cabal
  ( HSTool(..)
  , defaultMain
  )
  where

import Data.Version
import Data.List
import Data.Monoid
import Distribution.Simple.Compiler
import Distribution.Verbosity
import Distribution.InstalledPackageInfo
import Distribution.ParseUtils
import Distribution.Package
import Distribution.Text
import Distribution.ModuleName
import Options.Applicative
import Control.Monad
import Text.Printf
import Paths_haskell_packages as Our (version)

data HSTool = HSTool
  { toolName :: String
  , toolVersion :: Version
  , toolGetInstalledPkgs :: PackageDB -> IO [InstalledPackageInfo]
  , toolCompile :: FilePath -> [FilePath] -> IO ()
  , toolInstallLib
      :: FilePath
      -> FilePath
      -> Maybe FilePath
      -> PackageIdentifier
      -> [ModuleName]
      -> IO ()
    -- ^ the first three arguments are: build dir, target dir and optional
    -- target dir for dynamic libraries
  , toolRegister :: PackageDB -> InstalledPackageInfo -> IO ()
  }

defaultMain :: HSTool -> IO ()
defaultMain HSTool{..} =
  join $ execParser $ info (helper <*> optParser) idm
  where

  optParser =
    foldr (<|>) empty
      [ version
      , numericVersion
      , hspkgVersion
      , subparser pkgCommand
      , compiler]

  versionStr = showVersion toolVersion
  ourVersionStr = showVersion Our.version

  numericVersion =
    flag'
      (putStrLn versionStr)
      (long "numeric-version")

  hspkgVersion =
    flag'
      (putStrLn ourVersionStr)
      (long "hspkg-version")

  version =
    flag'
      (printf "%s %s\nBased on haskell-packages version %s\n" toolName versionStr ourVersionStr)
      (long "version")

  pkgCommand =
    command "pkg" (info (subparser pkgSubcommands) idm)
  pkgSubcommands = mconcat [pkgDump, pkgInstallLib, pkgRegister]

  pkgDump = command "dump" $ info (doDump <$> pkgDbStackParser) idm
    where
      doDump dbs = do
        pkgs <- concat <$> mapM toolGetInstalledPkgs dbs
        putStr $ intercalate "---\n" $ map showInstalledPackageInfo pkgs

  pkgInstallLib = command "install-library" $ flip info idm $
    toolInstallLib <$>
      (strOption (long "build-dir" & metavar "PATH")) <*>
      (strOption (long "target-dir" & metavar "PATH")) <*>
      (optional $ strOption (long "dynlib-target-dir" & metavar "PATH")) <*>
      (nullOption (long "package-id" & metavar "ID" & reader simpleParse)) <*>
      (arguments simpleParse (metavar "MODULE"))

  pkgRegister =
    command "register" $ flip info idm $
      doRegister <$> pkgDbParser

  doRegister d = do
    pi <- parseInstalledPackageInfo <$> getContents
    case pi of
      ParseOk _ a -> toolRegister d a
      ParseFailed e -> putStrLn $ snd $ locatedErrorMsg e

  compiler =
    toolCompile <$>
      (strOption (long "build-dir" & metavar "PATH") <|> pure ".") <*>
      arguments str (metavar "FILE")

pkgDbParser :: Parser PackageDB
pkgDbParser =
  flag' GlobalPackageDB (long "global") <|>
  flag' UserPackageDB   (long "user")   <|>
  (SpecificPackageDB <$> strOption (long "package-db" & metavar "PATH"))

pkgDbStackParser :: Parser PackageDBStack
pkgDbStackParser =
  (\fs -> if null fs then [GlobalPackageDB] else fs) <$>
    many pkgDbParser
