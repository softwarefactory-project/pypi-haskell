module Main (main) where

import Pypi (getVersion)

main :: IO ()
main = print $ getVersion "ansible"
