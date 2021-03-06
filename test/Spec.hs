module Main (main) where

import Data.Aeson (eitherDecode, decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isJust, fromJust)
import Data.Either (rights)
import Data.Versions (semver)
import Test.Tasty
import Test.Tasty.HUnit
import Pypi

main :: IO ()
main = do
  dataFile <- BSL.readFile "./test/data/sampleproject.json"
  defaultMain (tests dataFile)

tests :: ByteString -> TestTree
tests dataFile = testGroup "Tests" [encodingTests dataFile]

encodingTests :: ByteString -> TestTree
encodingTests dataFile =
  testGroup
    "FromJSON"
    [ testCase "Test sampleproject.json"
        $ assertBool "PypiProject is decoded"
        $ isProject (eitherDecode dataFile)
    , testCase "Get semvers"
        $ assertEqual "Semver are correct"
          (getReleaseSemVer sampleProject)
          (rights [semver "1.2.0"])
    ]
  where
    isProject :: Either String PypiProject -> Bool
    isProject (Right _) = True
    isProject (Left err) = error err
    sampleProject :: PypiProject
    sampleProject = fromJust $ decode dataFile
