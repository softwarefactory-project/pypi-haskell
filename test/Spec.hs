module Main (main) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isJust)
import Test.Tasty
import Test.Tasty.HUnit
import Pypi

main :: IO ()
main = do
  dataFile <- BSL.readFile "./test/data/ansible.json"
  defaultMain (tests dataFile)

tests :: ByteString -> TestTree
tests dataFile = testGroup "Tests" [encodingTests dataFile]

encodingTests :: ByteString -> TestTree
encodingTests dataFile =
  testGroup
    "FromJSON"
    [ testCase "Test ansible.json"
        $ assertBool "PypiProject is decoded"
        $ isProject (eitherDecode dataFile)
    ]
  where
    isProject :: Either String PypiProject -> Bool
    isProject (Right _) = True
    isProject (Left err) = error err
