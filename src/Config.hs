{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Configurator
  ( load
  , Worth(Required)
  , lookupDefault )
import Data.Configurator.Types (Config)

config :: IO Config
config = load [Required "config"]

port :: IO Int
port = config >>= (\c -> lookupDefault 8080 c "server.port")

apiPrefix :: IO String
apiPrefix = config >>= (\c -> lookupDefault "/api/" c "server.api_prefix")

dbFileName :: IO String
dbFileName = config >>=
  (\c -> lookupDefault "Database.db" c "database.filename")

verbose :: IO Int
verbose = config >>= (\c -> lookupDefault 0 c "server.verbose")
