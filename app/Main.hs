module Main (main) where

import Pypi (withClient, getProject)

main :: IO ()
main = withClient $ \client -> do
  project <- getProject "ansible" client
  print project
