-- | This module provides Cabal integration.
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.HaskellSuite.Cabal
  ( main, customMain )
  where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Foldable                      (asum)
import           Data.List
import           Data.Monoid
import           Data.Proxy
import           Data.Typeable
import qualified Distribution.HaskellSuite.Compiler as Compiler
import           Distribution.HaskellSuite.Packages
import           Distribution.InstalledPackageInfo  (parseInstalledPackageInfo,
                                                     showInstalledPackageInfo)
import           Distribution.ModuleName            hiding (main)
import           Distribution.Package
import           Distribution.ParseUtils
import           Distribution.Simple.Compiler
import           Distribution.Text
import           Distribution.Version
import           Language.Haskell.Exts.Extension
import           Options.Applicative
import           Options.Applicative.Types
import           Paths_haskell_packages             as Our (version)
import           System.Directory
import           System.FilePath
import           Text.Printf

-- It is actually important that we import 'defaultCpphsOptions' from
-- hse-cpp and not from cpphs, because they are different. hse-cpp version
-- provides the defaults more compatible with haskell-src-exts.
import           Language.Haskell.Exts.CPP

main
  :: forall c . Compiler.Is c
  => c -> IO ()
main = customMain empty

customMain
  :: forall c . Compiler.Is c
  => Parser (IO ())
  -> c -> IO ()
customMain additionalActions t =
  join $ customExecParser (prefs noBacktrack) $ info (helper <*> optParser) idm
  where

  optParser =
    asum
      [ version
      , compilerVersion
      , hspkgVersion
      , supportedLanguages
      , supportedExtensions
      , hsubparser $ pkgCommand <> compilerCommand
      , additionalActions
      ]

  versionStr = showVersion $ Compiler.version t
  ourVersionStr = showVersion (mkVersion' Our.version)

  compilerVersion =
    flag'
      (printf "%s %s\n" (Compiler.name t) versionStr)
      (long "compiler-version")

  hspkgVersion =
    flag'
      (putStrLn ourVersionStr)
      (long "hspkg-version")

  supportedLanguages =
    flag'
      (mapM_ (putStrLn . prettyLanguage) $ Compiler.languages t)
      (long "supported-languages")

  supportedExtensions =
    flag'
      (mapM_ (putStrLn . prettyExtension) $ Compiler.languageExtensions t)
      (long "supported-extensions")

  version =
    flag'
      (printf "%s %s\nBased on haskell-packages version %s\n" (Compiler.name t) versionStr ourVersionStr)
      (long "version")

  pkgCommand =
    command "pkg" (info (hsubparser pkgSubcommands) idm)
  pkgSubcommands =
    mconcat
      [ pkgDump
      , pkgInstallLib
      , pkgUpdate
      , pkgUnregister
      , pkgList
      , pkgInit
      ]

  pkgDump = command "dump" $ info (doDump <$> pkgDbStackParser) idm
    where
      doDump dbs = do
        pkgs <-
          fmap concat $
          forM dbs $ \db ->
            getInstalledPackages
              (Proxy :: Proxy (Compiler.DB c))
              db
        putStr $ intercalate "---\n" $ map showInstalledPackageInfo pkgs

  pkgInstallLib = command "install-library" $ flip info idm $
    Compiler.installLib t <$>
      strOption (long "build-dir" <> metavar "PATH") <*>
      strOption (long "target-dir" <> metavar "PATH") <*>
      optional (strOption (long "dynlib-target-dir" <> metavar "PATH")) <*>
      option (simpleParseM "package-id") (long "package-id" <> metavar "ID") <*>
      many (argument (simpleParseM "module") (metavar "MODULE"))

  pkgUpdate =
    command "update" $ flip info idm $
      doRegister <$> pkgDbParser

  doRegister d = do
    pi <- parseInstalledPackageInfo <$> getContents
    case pi of
      ParseOk _ a   -> Compiler.register t d a
      ParseFailed e -> putStrLn $ snd $ locatedErrorMsg e

  pkgUnregister =
    command "unregister" $ flip info idm $
      Compiler.unregister t <$> pkgDbParser <*> pkgIdParser

  pkgInit =
    command "init" $ flip info idm $
      initDB <$> argument str (metavar "PATH")

  pkgList =
    command "list" $ flip info idm $
      Compiler.list t <$> pkgDbParser

  compilerCommand =
    command "compile" (info compiler idm)
  compiler =
    (\srcDirs buildDir lang exts cppOpts pkg dbStack deps mods ->
        Compiler.compile t buildDir lang exts cppOpts pkg dbStack deps =<< findModules srcDirs mods) <$>
      many (strOption (short 'i' <> metavar "PATH")) <*>
      (strOption (long "build-dir" <> metavar "PATH") <|> pure ".") <*>
      optional (classifyLanguage <$> strOption (short 'G' <> metavar "language")) <*>
      many (parseExtension <$> strOption (short 'X' <> metavar "extension")) <*>
      cppOptsParser <*>
      option (simpleParseM "package name") (long "package-name" <> metavar "NAME-VERSION") <*>
      pkgDbStackParser <*>
      many (mkUnitId <$> strOption (long "package-id")) <*>
      many (argument str (metavar "MODULE"))

newtype ModuleNotFound = ModuleNotFound String
  deriving Typeable

instance Show ModuleNotFound where
  show (ModuleNotFound mod) = printf "Module %s not found" mod
instance Exception ModuleNotFound

findModules :: [FilePath] -> [String] -> IO [FilePath]
findModules srcDirs = mapM (findModule srcDirs)

findModule :: [FilePath] -> String -> IO FilePath
findModule srcDirs mod = do
  r <- runExceptT $ sequence_ (checkInDir <$> srcDirs <*> exts)
  case r of
    Left found -> return found
    Right {}   -> throwIO $ ModuleNotFound mod

  where
    exts = ["hs", "lhs"]

    checkInDir dir ext = ExceptT $ do
      let file = dir </> toFilePath (fromString mod) <.> ext
      found <- doesFileExist file
      return $ if found
        then Left file
        else Right ()

pkgDbParser :: Parser PackageDB
pkgDbParser =
  flag' GlobalPackageDB (long "global") <|>
  flag' UserPackageDB   (long "user")   <|>
  (SpecificPackageDB <$> strOption (long "package-db" <> metavar "PATH"))

pkgIdParser :: Parser PackageId
pkgIdParser =
  argument (simpleParseM "package-id") (metavar "PACKAGE")

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
              (_, [])      -> (str, "1")
              (sym, _:var) -> (sym, var)
          in Endo $ \opts -> opts { defines = def : defines opts }

    includeDir =
      flip fmap (strOption (short 'I' <> metavar "PATH")) $
      \str -> Endo $ \opts -> opts { includes = str : includes opts }

-- | 'simpleParse' is defined in "Distribution.Text" with type
--
-- >simpleParse :: Text a => String -> Maybe a
--
-- (It is similar to 'read'.)
--
-- 'simpleParseM' wraps it as a 'ReadM' value to be used for parsing
-- command-line options
simpleParseM :: Text a => String -> ReadM a
simpleParseM entityName = do
  str <- readerAsk
  case simpleParse str of
    Just thing -> return thing
    Nothing -> readerError $
      "could not parse " ++ entityName ++ " '" ++ str ++ "'"
