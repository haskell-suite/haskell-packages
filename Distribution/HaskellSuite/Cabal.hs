-- | This module provides Cabal integration.
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module Distribution.HaskellSuite.Cabal
  ( defaultMain )
  where

import Data.Typeable
import Data.Version
import Data.List
import Data.Monoid
import Data.Proxy
import Distribution.Simple.Compiler
import Distribution.InstalledPackageInfo
  ( showInstalledPackageInfo
  , parseInstalledPackageInfo )
import Distribution.ParseUtils
import Distribution.Package
import Distribution.Text
import Distribution.ModuleName
import Options.Applicative
import Control.Monad
import Control.Monad.Trans.Either
import Control.Exception
import Text.Printf
import qualified Distribution.HaskellSuite.Compiler as Compiler
import Distribution.HaskellSuite.PackageDB
import Language.Haskell.Exts.Annotated.CPP
import Language.Haskell.Exts.Extension
import Paths_haskell_packages as Our (version)
import System.FilePath
import System.Directory

defaultMain
  :: forall c . Compiler.Is c
  => c -> IO ()
defaultMain t =
  join $ execParser $ info (helper <*> optParser) idm
  where

  optParser =
    foldr (<|>) empty
      [ version
      , numericVersion
      , hspkgVersion
      , supportedExtensions
      , subparser pkgCommand
      , compiler]

  versionStr = showVersion $ Compiler.version t
  ourVersionStr = showVersion Our.version

  numericVersion =
    flag'
      (putStrLn versionStr)
      (long "numeric-version")

  hspkgVersion =
    flag'
      (putStrLn ourVersionStr)
      (long "hspkg-version")

  supportedExtensions =
    flag'
      (mapM_ (putStrLn . prettyExtension) $ Compiler.languageExtensions t)
      (long "supported-extensions")

  version =
    flag'
      (printf "%s %s\nBased on haskell-packages version %s\n" (Compiler.name t) versionStr ourVersionStr)
      (long "version")

  pkgCommand =
    command "pkg" (info (subparser pkgSubcommands) idm)
  pkgSubcommands = mconcat [pkgDump, pkgInstallLib, pkgRegister]

  pkgDump = command "dump" $ info (doDump <$> pkgDbStackParser) idm
    where
      doDump dbs = do
        pkgs <-
          fmap concat $
          forM dbs $ \db ->
            getInstalledPackages
              InitDB
              (Proxy :: Proxy (Compiler.DB c))
              db
        putStr $ intercalate "---\n" $ map showInstalledPackageInfo pkgs

  pkgInstallLib = command "install-library" $ flip info idm $
    Compiler.installLib t <$>
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
      ParseOk _ a -> Compiler.register t d a
      ParseFailed e -> putStrLn $ snd $ locatedErrorMsg e

  compiler =
    (\srcDirs buildDir exts cppOpts dbStack pkgids mods ->
        Compiler.compile t buildDir exts cppOpts dbStack pkgids =<< findModules srcDirs mods) <$>
      (many $ strOption (short 'i' & metavar "PATH")) <*>
      (strOption (long "build-dir" & metavar "PATH") <|> pure ".") <*>
      (many $ parseExtension <$> strOption (short 'X' & metavar "extension")) <*>
      cppOptsParser <*>
      pkgDbStackParser <*>
      (many $ InstalledPackageId <$> strOption (long "package-id")) <*>
      arguments str (metavar "MODULE")

data ModuleNotFound = ModuleNotFound String
  deriving Typeable

instance Show ModuleNotFound where
  show (ModuleNotFound mod) = printf "Module %s not found" mod
instance Exception ModuleNotFound

findModules srcDirs = mapM (findModule srcDirs)
findModule srcDirs mod = do
  r <- runEitherT $ sequence_ (checkInDir <$> srcDirs <*> exts)
  case r of
    Left found -> return found
    Right {} -> throwIO $ ModuleNotFound mod

  where
    exts = ["hs", "lhs"]

    checkInDir dir ext = EitherT $ do
      let file = dir </> toFilePath (fromString mod) <.> ext
      found <- doesFileExist file
      return $ if found
        then Left file
        else Right ()

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
