module Server (app) where

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl')
import Network.HTTP.Types (parseMethod)
import Server.Routes (routes, onError)

import qualified Data.Map as Map
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai

import Server.Types

app :: Wai.Application
app request responder = do
    result <- runExceptT $ router request
    case result of
      Right response -> responder response
      Left err -> responder $ onError err

router :: Wai.Request -> ServerM Wai.Response
router request =
    case reqMethod of
      Right method -> do
        (params, _) <- liftIO reqBody
        routes reqPath method $ mapParams params
      Left _ -> throwError UnknownHttpMethod
  where
    reqBody = Wai.parseRequestBody Wai.lbsBackEnd request
    reqMethod = parseMethod $ Wai.requestMethod request
    reqPath = Wai.pathInfo request

-- helper functions

mapParams :: [Wai.Param] -> Params
mapParams = foldl' insert Map.empty
  where
    insert params (name, val) = Map.insert name val params
