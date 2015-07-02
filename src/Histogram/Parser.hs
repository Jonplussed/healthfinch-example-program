module Histogram.Parser (urlTextHist) where

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

import Histogram.Types

lynxOptions :: [String]
lynxOptions =
    [ "-dump"       -- send doc text to STDOUT
    , "-notitle"    -- don't display the page title
    , "-nolist"     -- don't list links at end of output
    , "-validate"   -- allow only http(s) URIs
    ]

urlTextHist :: MonadIO m => String -> m (Either ParseError Histogram)
urlTextHist url = do
    (exitCode, hist) <- sourceProcessWithConsumer (lynxOutput url) parseText
    return $ case exitCode of
      ExitSuccess -> Right hist
      _ -> Left PageReadError

-- conduits

parseText :: Monad m => Consumer ByteString m Histogram
parseText = eachWord =$= normalize =$= tabulate

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

lynxOutput :: String -> CreateProcess
lynxOutput url = proc "lynx" $ lynxOptions ++ [url]
