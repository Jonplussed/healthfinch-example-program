module Server (app) where

import Data.List (foldl')
import Network.HTTP.Types (Param)
import Server.Routes (error400, router)

import qualified Data.Map as Map
import qualified Network.WAI as Wai
import qualified Network.WAI.Parse as Wai

import Server.Types

app :: Wai.Application
app req = case reqMethod of
    Right method -> do
      (params, _) <- requestBody
      router reqPath method $ mapParams params
    Left _ -> error400
  where
    reqBody = Wai.parseRequestBody Wai.lbsBackEnd req
    reqMethod = Wai.parseMethod $ Wai.reqMethod req
    reqPath = Wai.pathInfo req

-- helper functions

mapParams :: [Param] -> Params
mapParams = foldl' insert Map.empty
  where
    insert params (name, val) = M.insert name val params
