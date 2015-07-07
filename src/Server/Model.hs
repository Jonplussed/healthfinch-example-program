{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Server.Model where

import Data.Text (Text)
import Hasql.Postgres (Postgres)

import qualified Hasql as Db

import Server.Types

createWord :: Text -> Text -> Int -> Db.Tx Postgres s ()
createWord url word count =
    Db.unitEx $ [Db.stmt|
      INSERT INTO histogram (url, word, count)
      VALUES (?,?,?)
    |] url word count

listWordsForUrl :: Text -> Db.Tx Postgres s [(Text, Int)]
listWordsForUrl url =
    Db.listEx $ [Db.stmt|
      SELECT (word, count)
      FROM histogram
      WHERE url = ?
      ORDER BY count
    |] url
