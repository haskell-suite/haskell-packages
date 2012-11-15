-- | This module provides Cabal integration.
{-# LANGUAGE RecordWildCards #-}

module Distribution.HaskellSuite.Cabal where

import Data.Version
import Data.List
import Data.Monoid
import Distribution.Simple.Compiler
import Distribution.Verbosity
import Distribution.InstalledPackageInfo
import Options.Applicative
import Control.Monad
import Text.Printf

-- FIXME
ourVersion = "0.1"

data HSTool = HSTool
  { toolName :: String
  , toolVersion :: Version
  , toolGetInstalledPkgs :: PackageDB -> IO [InstalledPackageInfo]
  }

main = defaultMain testTool
  where
  testTool = HSTool
    { toolName = "myTool"
    , toolVersion = Version [3,1,4] []
    , toolGetInstalledPkgs = \_ -> return [emptyInstalledPackageInfo, emptyInstalledPackageInfo]
    }

defaultMain :: HSTool -> IO ()
defaultMain HSTool{..} =
  join $ execParser $ info (helper <*> optParser) idm
  where

  optParser =
    foldr (<|>) empty [version, hspkgVersion, subparser pkgCommand]

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
    command "pkg" (info (subparser pkgSubcommands <*> pkgDbParser) idm)
  pkgSubcommands = mconcat [pkgDump]

  pkgDump =
    let doDump dbs = do
          pkgs <- concat <$> mapM toolGetInstalledPkgs dbs
          putStr $ intercalate "---\n" $ map showInstalledPackageInfo pkgs
    in command "dump" $ info (pure doDump) idm

pkgDbParser :: Parser PackageDBStack
pkgDbParser =
  (\fs -> if null fs then [GlobalPackageDB] else fs) <$>
    many (
      flag' GlobalPackageDB (long "global") <|>
      flag' UserPackageDB   (long "user")   <|>
      (SpecificPackageDB <$> strOption (long "package-db" & metavar "PATH")))
