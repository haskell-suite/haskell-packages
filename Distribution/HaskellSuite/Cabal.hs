-- | This module provides Cabal integration.
{-# LANGUAGE RecordWildCards #-}

module Distribution.HaskellSuite.Cabal where

import Data.Version
import Data.List
import Data.Monoid
import Distribution.Simple.Compiler
import Distribution.Verbosity
import Distribution.InstalledPackageInfo
import Distribution.ParseUtils
import Options.Applicative
import Control.Monad
import Text.Printf

-- FIXME
ourVersion = "0.1"

data HSTool = HSTool
  { toolName :: String
  , toolVersion :: Version
  , toolGetInstalledPkgs :: PackageDB -> IO [InstalledPackageInfo]
  , toolCompile :: FilePath -> [FilePath] -> IO ()
  , toolRegister :: PackageDB -> InstalledPackageInfo -> IO ()
  }

defaultMain :: HSTool -> IO ()
defaultMain HSTool{..} =
  join $ execParser $ info (helper <*> optParser) idm
  where

  optParser =
    foldr (<|>) empty [version, hspkgVersion, subparser pkgCommand, compiler]

  versionStr = showVersion toolVersion

  hspkgVersion =
    flag'
      (putStrLn ourVersion)
      (long "hspkg-version")

  version =
    flag'
      (printf "%s %s\nBased on haskell-packages version %s\n" toolName versionStr ourVersion)
      (long "version")

  pkgCommand =
    command "pkg" (info (subparser pkgSubcommands) idm)
  pkgSubcommands = mconcat [pkgDump, pkgRegister]

  pkgDump = command "dump" $ info (doDump <$> pkgDbStackParser) idm
    where
      doDump dbs = do
        pkgs <- concat <$> mapM toolGetInstalledPkgs dbs
        putStr $ intercalate "---\n" $ map showInstalledPackageInfo pkgs

  pkgRegister =
    command "register" $ flip info idm $
      doRegister <$> pkgDbParser

  pkgUpdate =
    -- FIXME: this should be a separate tool
    command "update" $ flip info idm $
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
