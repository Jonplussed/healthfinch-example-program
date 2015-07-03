module Server.Types where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import Network.URI (URI)

type Params = Map ByteString ByteString

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
