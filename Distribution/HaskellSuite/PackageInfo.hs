{-# LANGUAGE OverloadedStrings #-}
module Distribution.HaskellSuite.PackageInfo where

import Data.Aeson
import Control.Applicative

data PackageInfo = PackageInfo
  { pkgName :: String
  , pkgVersion :: String
  , pkgModules :: [String]
  }

instance ToJSON PackageInfo where
  toJSON (PackageInfo name ver mods) = object
    [ "name" .= name
    , "version" .= ver
    , "modules" .= mods
    ]

instance FromJSON PackageInfo where
  parseJSON (Object v) =
    PackageInfo <$>
      v .: "name" <*>
      v .: "version" <*>
      v .: "modules"
  parseJSON _ = empty
