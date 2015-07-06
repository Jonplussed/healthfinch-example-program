module Histogram
( urlTextHistogram
) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum, toLower)
import Data.Conduit (Conduit, Consumer, (=$=))
import Data.Conduit.Process (sourceProcessWithConsumer)
import Data.Text (Text)
import Network.URI (URI)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess, proc)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Conduit.List as Con
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

type Histogram = Map.Map Text Int

urlTextHistogram :: MonadIO m => URI -> m (Maybe Histogram)
urlTextHistogram url = do
    (exitCode, histogram) <- sourceProcessWithConsumer (lynxOutput url) parser
    case exitCode of
      ExitSuccess -> return $ Just histogram
      _ -> return Nothing

-- process for retrieving URL text

lynxOptions :: [String]
lynxOptions =
    [ "-dump"       -- send doc text to STDOUT
    , "-notitle"    -- don't display the page title
    , "-nolist"     -- don't list links at end of output
    , "-validate"   -- allow only http(s) URIs
    ]

lynxOutput :: URI -> CreateProcess
lynxOutput url = proc "lynx" $ lynxOptions ++ [show url]

-- conduits

parser :: Monad m => Consumer ByteString m Histogram
parser = eachWord =$= tabulate

eachWord :: Monad m => Conduit ByteString m ByteString
eachWord = Con.concatMap $ C8.splitWith (not . isAlphaNum)

tabulate :: Monad m => Consumer ByteString m Histogram
tabulate = Con.fold tabWord Map.empty

-- helper functions

tabWord :: Histogram -> ByteString -> Histogram
tabWord histogram word
    | C8.null word = histogram
    | otherwise = Map.alter addOne (normalize word) histogram
  where
    addOne = maybe (Just 1) (Just . succ)

normalize :: ByteString -> Text
normalize = Text.decodeUtf8With (Text.replace '?') . C8.map toLower
