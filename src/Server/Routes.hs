{-# LANGUAGE OverloadedStrings #-}

module Server.Routes
( routes
, onError
) where

import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Network.HTTP.Types (StdMethod (..))

import qualified Server.Controller as App
import qualified Server.Error as App
import qualified Network.Wai as Wai

import Server.Types

routes :: [Text] -> StdMethod -> Params -> ServerM Wai.Response
routes []               GET  = App.indexAction
routes ["urls"]         POST = App.createAction
routes ["urls", urlId]  GET  = App.showAction (fromText urlId)
routes _                _    = const (lift $ throwError UnknownRoute)

onError :: ServerError -> Wai.Response
onError _  = App.error404
