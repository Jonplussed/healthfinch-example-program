{-# LANGUAGE OverloadedStrings #-}

module Server.Routes
( routes
, onError
) where

import Control.Monad.Except (throwError)
import Data.Text (Text)
import Network.HTTP.Types (StdMethod (..))

import qualified Server.Controller as App
import qualified Network.Wai as Wai

import Server.Types


routes :: [Text] -> StdMethod -> Params -> Response
routes []               GET  _      = App.homePath
routes ["urls"]         GET  _      = App.indexPath
routes ["urls"]         POST params = App.createPath params
routes ["urls", urlId]  GET  _      = App.showPath $ fromText urlId
routes _                _    _      = throwError UnknownRoute

onError :: ServerError -> Wai.Response
onError _  = App.error404Path
