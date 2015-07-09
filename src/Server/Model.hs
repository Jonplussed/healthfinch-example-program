{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Server.Model
( createHistogram
, listHistogramWords
, doesHistogramExist
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

listHistogramWords :: URI -> Db.Tx Postgres s [(Text, Int)]
listHistogramWords = listWordsSql . urlText

doesHistogramExist :: URI -> Db.Tx Postgres s Bool
doesHistogramExist url = do
    exists <- testForUrlSql $ urlText url
    case exists of
      Just _ -> return True
      _ -> return False

-- queries

createWordSql :: Text -> Text -> Int -> Db.Tx Postgres s ()
createWordSql url word frequency =
    Db.unitEx [Db.stmt|
      INSERT INTO histogram (url, word, frequency)
      VALUES ($url, $word, $frequency)
    |]

listWordsSql :: Text -> Db.Tx Postgres s [(Text, Int)]
listWordsSql url =
    Db.listEx [Db.stmt|
      SELECT word, frequency
      FROM histogram
      WHERE url = $url
      ORDER BY frequency DESC
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
