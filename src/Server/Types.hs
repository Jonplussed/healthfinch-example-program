module Server.Types where

import Control.Monad.Except (ExceptT)
import Data.Map (Map)
import Network.URI (URI)
import Text.Blaze (ToMarkup, toMarkup)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as Text
import qualified Network.Wai as Wai

type Params = Map C8.ByteString C8.ByteString
type Response = ExceptT ServerError IO Wai.Response

newtype UrlId = UrlId Int
newtype WordCountId = WordCountId Int

data ServerError
  = UnknownRoute
  | UnknownHttpMethod
  | InvalidUrl
  | CannotCreateHistogram

data UrlEntry = UrlEntry
  { u_id :: UrlId
  , u_uri :: URI
  }

data WordCountEntry = WordCountEntry
  { wc_id :: WordCountId
  , wc_urlId :: UrlId
  , wc_word :: Text.Text
  , wc_count :: Int
  }

class FromText a where
  fromText :: Text.Text -> a

instance FromText UrlId where
  fromText = UrlId . read . Text.unpack

instance ToMarkup C8.ByteString where
  toMarkup = toMarkup . C8.unpack
