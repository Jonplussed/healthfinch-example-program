{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Server.Model.Histogram
( createHistogram
, doesHistogramExist
, listHistogramWords
, listUrls
) where

import Data.Text (Text, pack)
import Hasql.Postgres (Postgres)
import Histogram (Histogram)
import Network.URI (URI)

import qualified Data.Map as Map
import qualified Hasql as Db

createHistogram :: URI -> Histogram -> Db.Tx Postgres s ()
createHistogram url histogram =
    mapM_ create $ Map.toList histogram
  where
    create (word, freq) = createWordSql (urlText url) word freq

doesHistogramExist :: URI -> Db.Tx Postgres s Bool
doesHistogramExist url = do
    exists <- testForUrlSql $ urlText url
    case exists of
      Just _ -> return True
      _ -> return False

listHistogramWords :: URI -> Db.Tx Postgres s [(Text, Int)]
listHistogramWords = listWordsSql . urlText

listUrls :: Db.Tx Postgres s [(Text, Text, Int)]
listUrls = listUrlsSql

-- queries

createWordSql :: Text -> Text -> Int -> Db.Tx Postgres s ()
createWordSql url word frequency =
    Db.unitEx [Db.stmt|
      INSERT INTO histogram (url, word, frequency)
      VALUES ($url, $word, $frequency)
    |]

listUrlsSql :: Db.Tx Postgres s [(Text, Text, Int)]
listUrlsSql =
    Db.listEx [Db.stmt|
      SELECT DISTINCT url, most.word, most.frequency
      FROM histogram hist, LATERAL (
        SELECT word, frequency
        FROM histogram
        WHERE url = hist.url
        ORDER BY frequency DESC
        LIMIT 1
      ) most
      ORDER BY url
    |]

listWordsSql :: Text -> Db.Tx Postgres s [(Text, Int)]
listWordsSql url =
    Db.listEx [Db.stmt|
      SELECT word, frequency
      FROM histogram
      WHERE url = $url
      ORDER BY frequency DESC
      LIMIT 10
    |]

testForUrlSql :: Text -> Db.Tx Postgres s (Maybe (Text, Int))
testForUrlSql url =
    Db.maybeEx [Db.stmt|
      SELECT word, frequency
      FROM histogram
      WHERE url = $url
      LIMIT 1
    |]

-- helpers

urlText :: URI -> Text
urlText = pack . show
