module Words.Parser (tabUrlText) where

import Data.ByteString (ByteString)
import Data.Char (isAlphaNum, toLower)
import Data.Conduit (Conduit, Consumer, ($$), (=$=), await, yield)
import Data.Conduit.Process (sourceProcessWithConsumer)
import Data.List (foldl')
import Control.Monad.IO.Class (MonadIO)
import System.Exit (ExitCode)
import System.Process (CreateProcess, proc)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Conduit.List as Con
import qualified Data.Map as Map

type Histogram = Map.Map ByteString Int

tabUrlText :: MonadIO m => String -> m (ExitCode, Histogram)
tabUrlText url =
  sourceProcessWithConsumer (readUrlProc url) processText

-- conduits

processText :: Monad m => Consumer ByteString m Histogram
processText = eachWord =$= normalize =$= tabulate

eachWord :: Monad m => Conduit ByteString m ByteString
eachWord = Con.concatMap C8.words

normalize :: Monad m => Conduit ByteString m ByteString
normalize = Con.map $ C8.map toLower . C8.filter isAlphaNum

tabulate :: Monad m => Consumer ByteString m Histogram
tabulate = Con.fold (\hist word -> Map.alter addOne word hist) Map.empty
  where
    addOne (Just n) = Just $ succ n
    addOne _ = Just 1

-- helpers

readUrlProc :: String -> CreateProcess
readUrlProc url = proc "lynx" ["-dump", "-notitle", "-nolist", url]
