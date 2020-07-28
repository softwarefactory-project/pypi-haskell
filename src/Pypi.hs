-- | This module contains the pypi REST client
module Pypi
  ( -- * Client
    PypiClient (baseUrl),
    withClient,

    -- * Api
    getProject,

    -- * Main data types
    PypiProject (..),
  )
where

import Data.Aeson (FromJSON, decode, eitherDecode)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text (Text, unpack)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Pypi.Project

-- | The PypiClient record, use 'withClient' to create
data PypiClient
  = PypiClient
      { -- | the base url
        baseUrl :: Text,
        manager :: Manager
      }

-- | Create the 'PypiClient'
withClient ::
  -- | The callback
  (PypiClient -> IO ()) ->
  -- | withClient performs the IO
  IO ()
withClient callBack =
  do
    manager <- newManager tlsManagerSettings
    callBack (PypiClient {..})
  where
    baseUrl = "https://pypi.org/pypi/"

pypiGet ::
  (FromJSON a) =>
  Text ->
  PypiClient ->
  IO a
pypiGet path PypiClient {..} =
  do
    request <- parseUrlThrow (unpack $ baseUrl <> path)
    response <- httpLbs request manager
    case eitherDecode $ responseBody response of
      Left err -> error $ "Decoding of " <> show (responseBody response) <> " failed with: " <> err
      Right a -> pure a

-- | Get project infos
getProject :: Text -> PypiClient -> IO PypiProject
getProject project = pypiGet (project <> "/json")
