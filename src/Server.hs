module Server (app) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.List (foldl')
import Server.Routes (routes, onError)

import qualified Data.Map as Map
import qualified Hasql as Db
import qualified Hasql.Postgres as Db
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai

import Server.Types

app :: Db.Pool Db.Postgres -> Wai.Application
app db request responder = do
    result <- runExceptT $ withDbConn db $ router request
    case result of
      Right response -> responder response
      Left err -> responder $ onError err

withDbConn :: Db.Pool Db.Postgres -> ServerM a -> ExceptT ServerError IO a
withDbConn db more = do
    result <- Db.session db more
    case result of
      Right r -> return r
      _ -> throwError PostgresError

router :: Wai.Request -> ServerM Wai.Response
router request = do
    (method, paramList) <- requestParams request
    routes method (Wai.pathInfo request) (paramListToMap paramList)

-- NOTE: the HTTP spec does not describe how conflicting query parameters and
-- POST body parameter should be handled. I've elected to ignore all query
-- parameters during a POST request.
requestParams :: Wai.Request -> ServerM (Http.StdMethod, [Wai.Param])
requestParams request =
    case Http.parseMethod $ Wai.requestMethod request of
      Right m@Http.GET ->
        let params = Http.parseSimpleQuery $ Wai.rawQueryString request in
        return (m, params)
      Right m@Http.POST -> do
        (params, _) <- liftIO $ Wai.parseRequestBody Wai.lbsBackEnd request
        return (m, params)
      Right m ->
        return (m, [])
      _ ->
        lift $ throwError UnknownHttpMethod

-- helper functions

paramListToMap :: [Wai.Param] -> Params
paramListToMap = foldl' insert Map.empty
  where
    insert params (name, val) = Map.insert name val params
