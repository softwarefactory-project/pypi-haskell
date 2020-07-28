module Main (main) where

import System.Environment (getArgs)
import qualified Data.Text as T
import Control.Monad (forM_)
import Pypi (withClient, getProject, getReleaseSemVer)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [project] -> printSemVers (T.pack project)
    _ -> putStrLn "usage: pypi-cli project-name"

printSemVers :: T.Text -> IO ()
printSemVers projectName =
  withClient $ \client -> do
    project <- getProject projectName client
    print $ projectName <> " versions:"
    forM_ (getReleaseSemVer project) print
