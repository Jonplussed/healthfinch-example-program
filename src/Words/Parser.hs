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

tabUrlText :: MonadIO m => String -> m (ExitCode, Map.Map ByteString Int)
tabUrlText url = sourceProcessWithConsumer (readUrlProc url) processText

-- helpers

processText :: Monad m => Consumer ByteString m (Map.Map ByteString Int)
processText = await >>= return . maybe Map.empty process
  where
    process = tabulate . map normalize . C8.words

normalize :: ByteString -> ByteString
normalize = C8.map toLower . C8.filter isAlphaNum

tabulate :: [ByteString] -> Map.Map ByteString Int
tabulate = foldl' (flip (Map.alter addOne)) Map.empty
  where
    addOne (Just n) = Just $ succ n
    addOne _ = Just $ 1

readUrlProc :: String -> CreateProcess
readUrlProc url = proc "lynx" ["-dump", "-notitle", "-nolist", url]
