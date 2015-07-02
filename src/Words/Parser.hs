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
  sourceProcessWithConsumer (readUrlProc url) (processText =$= checkResults)

-- conduits

processText :: Monad m => Conduit ByteString m Histogram
processText = Con.map (tabulate . map normalize . C8.words)

checkResults :: Monad m => Consumer Histogram m Histogram
checkResults = await >>= return . maybe Map.empty id

-- helpers

normalize :: ByteString -> ByteString
normalize = C8.map toLower . C8.filter isAlphaNum

tabulate :: [ByteString] -> Histogram
tabulate = foldl' (flip (Map.alter addOne)) Map.empty
  where
    addOne (Just n) = Just $ succ n
    addOne _ = Just $ 1

readUrlProc :: String -> CreateProcess
readUrlProc url = proc "lynx" ["-dump", "-notitle", "-nolist", url]
