module Histogram (urlTextHistogram) where

import Data.ByteString (ByteString)
import Data.Char (isAlphaNum, toLower)
import Data.Conduit (Conduit, Consumer, (=$=))
import Data.Conduit.Process (sourceProcessWithConsumer)
import Control.Monad.IO.Class (MonadIO)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess, proc)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Conduit.List as Con
import qualified Data.Map as Map

type Histogram = Map.Map ByteString Int

urlTextHistogram :: MonadIO m => String -> m [(ByteString, Int)]
urlTextHistogram url = do
    (exitCode, list) <- sourceProcessWithConsumer (lynxOutput url) parseText
    return $ case exitCode of
      ExitSuccess -> Just list
      _ -> Nothing

-- process for retrieving URL text

lynxOptions :: [String]
lynxOptions =
    [ "-dump"       -- send doc text to STDOUT
    , "-notitle"    -- don't display the page title
    , "-nolist"     -- don't list links at end of output
    , "-validate"   -- allow only http(s) URIs
    ]

lynxOutput :: String -> CreateProcess
lynxOutput url = proc "lynx" $ lynxOptions ++ [url]

-- conduits

parseText :: Monad m => Consumer ByteString m [(ByteString, Int)]
parseText = eachWord =$= normalize =$= tabulate =$= toOrderedList

eachWord :: Monad m => Conduit ByteString m ByteString
eachWord = Con.concatMap C8.words

normalize :: Monad m => Conduit ByteString m ByteString
normalize = Con.map $ C8.map toLower . C8.filter isAlphaNum

tabulate :: Monad m => Consumer ByteString m Histogram
tabulate = Con.fold (\hist word -> Map.alter addOne word hist) Map.empty
  where
    addOne (Just n) = Just $ succ n
    addOne _ = Just 1

toOrderedList :: Monad m => Consumer Histogram m [(ByteString, Int)]
toOrderedList = Con.map $ sortByAmount . Map.toList
  where
    sortByAmount = sortBy (\(_,x) (_,y) -> compare x y)
