{-# LANGUAGE OverloadedStrings #-}

module Server.Routes
( routes
, onError
) where

import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Network.HTTP.Types (StdMethod (..))

import qualified Server.Controller as Cont
import qualified Server.Error as Err
import qualified Network.Wai as Wai

import Server.Types

routes :: StdMethod -> [Text] -> Params -> ServerM Wai.Response
routes GET  []            = Cont.indexAction
routes POST ["histogram"] = Cont.createAction
routes GET  ["histogram"] = Cont.showAction
routes _    _             = throw404Error

onError :: ServerError -> Wai.Response
onError UnknownRoute = Err.error404
onError err = Err.error500 err

-- helpers

throw404Error :: Params -> ServerM Wai.Response
throw404Error _ = lift $ throwError UnknownRoute
