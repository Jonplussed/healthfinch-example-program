module Histogram.Types where

import Data.ByteString (ByteString)
import Data.Map (Map)

type Histogram = Map ByteString Int

data ParseError
  = InvalidUrl String
  | PageReadError
  deriving (Show)
