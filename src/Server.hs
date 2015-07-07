module Server (app) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.List (foldl')
import Server.Routes (routes, onError)

import qualified Data.Map as Map
import qualified Hasql as Db
import qualified Hasql.Postgres as Db
import qualified Network.HTTP.Types as Net
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai

import Server.Types

app :: Db.Pool Db.Postgres -> Wai.Application
app db request responder = do
    result <- runExceptT $ withSession db $ router request
    case result of
      Right response -> responder response
      Left err -> responder $ onError err

withSession :: Db.Pool Db.Postgres -> ServerM a -> ExceptT ServerError IO a
withSession db more = do
    result <- Db.session db more
    case result of
      Right r -> return r
      _       -> throwError PostgresError

router :: Wai.Request -> ServerM Wai.Response
router request =
    case method of
      Right m -> do
        postParams <- liftIO $ postParamList request
        routes path m . mapParams $ postParams ++ queryParams
      _ -> lift $ throwError UnknownHttpMethod
  where
    method = Net.parseMethod $ Wai.requestMethod request
    path = Wai.pathInfo request
    queryParams = Net.parseSimpleQuery $ Wai.rawQueryString request

-- helper functions

postParamList :: Wai.Request -> IO [Wai.Param]
postParamList request = do
    (params, _) <- Wai.parseRequestBody Wai.lbsBackEnd request
    return params

mapParams :: [Wai.Param] -> Params
mapParams = foldl' insert Map.empty
  where
    insert params (name, val) = Map.insert name val params
