module Server (app) where

import Data.List (foldl')
import Network.HTTP.Types (parseMethod)
import Server.Routes (error400, router)

import qualified Data.Map as Map
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai

import Server.Types

app :: Wai.Application
app req resp = case reqMethod of
    Right method -> do
      (params, _) <- reqBody
      router reqPath method (mapParams params) resp
    Left _ -> error400 resp
  where
    reqBody = Wai.parseRequestBody Wai.lbsBackEnd req
    reqMethod = parseMethod $ Wai.requestMethod req
    reqPath = Wai.pathInfo req

-- helper functions

mapParams :: [Wai.Param] -> Params
mapParams = foldl' insert Map.empty
  where
    insert params (name, val) = Map.insert name val params
