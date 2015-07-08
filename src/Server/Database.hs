{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Server.Database where

import Data.Maybe (fromJust)

import qualified Hasql as Db
import qualified Hasql.Postgres as Db

import Server.Types

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

query :: (forall s. Db.Tx Db.Postgres s a) -> ServerM a
query = Db.tx (Just (txLevel, writeable))
  where
    txLevel = Db.ReadCommitted
    writeable = Just True
