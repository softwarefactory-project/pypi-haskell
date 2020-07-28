{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | The project data type
module Pypi.Project
  ( PypiProject (..),
  )
where

import Data.Aeson (FromJSON, Options (fieldLabelModifier), defaultOptions, genericParseJSON, parseJSON)
import Data.Char (isUpper, toLower)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data PypiProjectInfo
  = PypiProjectInfo
      { ppiAuthor :: Text,
        ppiAuthorEmail :: Text,
        ppiClassifiers :: [Text],
        ppiLicense :: Text
      }
  deriving (Eq, Show, Ord, Generic)

data PypiProjectRelease
  = PypiProjectRelease
      { pprFilename :: Text,
        pprPackagetype :: Text
      }
  deriving (Eq, Show, Ord, Generic)

data PypiProject
  = PypiProject
      { ppInfo :: PypiProjectInfo,
        ppReleases :: M.Map Text [PypiProjectRelease]
      }
  deriving (Eq, Show, Ord, Generic)

-- Convert ppiAuthorEmail to author_email
pypiParseJSON :: String -> Options
pypiParseJSON prefix = defaultOptions {fieldLabelModifier = recordToJson}
  where
    recordToJson = updateCase . drop (length prefix)
    updateCase [] = []
    updateCase (x : xs) = toLower x : updateCase' xs
    updateCase' [] = []
    updateCase' (x : xs)
      | isUpper x = '_' : toLower x : updateCase' xs
      | otherwise = x : updateCase' xs

instance FromJSON PypiProjectInfo where
  parseJSON = genericParseJSON $ pypiParseJSON "ppi"

instance FromJSON PypiProjectRelease where
  parseJSON = genericParseJSON $ pypiParseJSON "ppr"

instance FromJSON PypiProject where
  parseJSON = genericParseJSON $ pypiParseJSON "pp"
