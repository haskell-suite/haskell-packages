-- | This module provides Cabal integration.

module Distribution.HaskellSuite.Cabal
  ( defaultMain )
  where

import Data.Version
import Data.List
import Data.Monoid
import Distribution.Simple.Compiler
import Distribution.Verbosity
import Distribution.InstalledPackageInfo
  ( showInstalledPackageInfo
  , parseInstalledPackageInfo )
import Distribution.ParseUtils
import Distribution.Package
import Distribution.Text
import Distribution.ModuleName
import Options.Applicative
import Control.Monad
import Text.Printf
import Distribution.HaskellSuite.Tool
import Language.Preprocessor.Cpphs
import Paths_haskell_packages as Our (version)

defaultMain :: Tool tool => tool -> IO ()
defaultMain t =
  join $ execParser $ info (helper <*> optParser) idm
  where

  optParser =
    foldr (<|>) empty
      [ version
      , numericVersion
      , hspkgVersion
      , subparser pkgCommand
      , compiler]

  versionStr = showVersion $ toolVersion t
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
      (printf "%s %s\nBased on haskell-packages version %s\n" (toolName t) versionStr ourVersionStr)
      (long "version")

  pkgCommand =
    command "pkg" (info (subparser pkgSubcommands) idm)
  pkgSubcommands = mconcat [pkgDump, pkgInstallLib, pkgRegister]

  pkgDump = command "dump" $ info (doDump <$> pkgDbStackParser) idm
    where
      doDump dbs = do
        pkgs <- concat <$> mapM (toolGetInstalledPkgs t) dbs
        putStr $ intercalate "---\n" $ map showInstalledPackageInfo pkgs

  pkgInstallLib = command "install-library" $ flip info idm $
    toolInstallLib t <$>
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
      ParseOk _ a -> toolRegister t d a
      ParseFailed e -> putStrLn $ snd $ locatedErrorMsg e

  compiler =
    toolCompile t <$>
      (strOption (long "build-dir" & metavar "PATH") <|> pure ".") <*>
      cppOptsParser <*>
      pkgDbStackParser <*>
      (many $ InstalledPackageId <$> strOption (long "package-id")) <*>
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

cppOptsParser :: Parser CpphsOptions
cppOptsParser = appEndo <$> allMod <*> pure defaultCpphsOptions
  where
    allMod = fmap mconcat $ many $ define <|> includeDir

    define =
      flip fmap (strOption (short 'D' <> metavar "sym[=var]")) $
      \str ->
        let
          def :: (String, String)
          def =
            case span (/= '=') str of
              (_, []) -> (str, "")
              (sym, _:var) -> (sym, var)
          in Endo $ \opts -> opts { defines = def : defines opts }

    includeDir =
      flip fmap (strOption (short 'I' <> metavar "PATH")) $
      \str -> Endo $ \opts -> opts { includes = str : includes opts }
