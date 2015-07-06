{-# LANGUAGE OverloadedStrings #-}

module Server.Database where

import Data.Maybe (fromJust)
import qualified Hasql as Db
import qualified Hasql.Postgres as Db

poolConfig :: Db.PoolSettings
poolConfig = fromJust $ Db.poolSettings maxConns timeToLiveSecs
  where
    maxConns = 4
    timeToLiveSecs = 5

dbConfig :: Db.Settings
dbConfig = Db.ParamSettings host port user pass name
  where
    host = "localhost"
    port = 5432
    user = ""
    pass = ""
    name = "histogram_dev"
