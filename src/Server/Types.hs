module Server.Types
( Params
, ServerM
, ServerError (..)
) where

import Control.Monad.Except (ExceptT)
import Data.Map (Map)
import Hasql (Session)
import Hasql.Postgres (Postgres)

import qualified Data.ByteString.Char8 as C8

type Params = Map C8.ByteString C8.ByteString
type ServerM a = Session Postgres (ExceptT ServerError IO) a

data ServerError
  = UnknownRoute
  | UnknownHttpMethod
  | LynxError
  | InvalidParam C8.ByteString
  | PostgresError String
  deriving Show
