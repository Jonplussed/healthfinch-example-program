module Server.Types where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text, unpack)
import Network.URI (URI)
import Network.Wai (Response, ResponseReceived)

type Params = Map ByteString ByteString
type Responder = (Response -> IO ResponseReceived) -> IO ResponseReceived

newtype UrlId = UrlId Int
newtype WordCountId = WordCountId Int

data UrlEntry = UrlEntry
  { u_id :: UrlId
  , u_uri :: URI
  }

data WordCountEntry = WordCountEntry
  { wc_id :: WordCountId
  , wc_urlId :: UrlId
  , wc_word :: Text
  , wc_count :: Int
  }

class FromPathSegment a where
  fromPathSegment :: Text -> a

instance FromPathSegment UrlId where
  fromPathSegment = UrlId . read . unpack
