module Server.Types where

import Control.Monad.Except (ExceptT)
import Data.Map (Map)
import Network.URI (URI)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as Text
import qualified Hasql as Db
import qualified Hasql.Postgres as Db
import qualified Network.Wai as Wai

type Params = Map C8.ByteString C8.ByteString
type ServerM a = Db.Session Db.Postgres (ExceptT ServerError IO) a

data ServerError
  = UnknownRoute
  | UnknownHttpMethod
  | LynxError
  | InvalidParam C8.ByteString
  | MissingParam C8.ByteString
  | PostgresError
  deriving Show
