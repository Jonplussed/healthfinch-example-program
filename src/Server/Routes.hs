{-# LANGUAGE OverloadedStrings #-}

module Server.Routes
( router
, error400
, error404
) where

import Data.Text (Text)
import Network.HTTP.Types (StdMethod (..))
import Network.WAI (Response)

import qualified Server.Controller as App

import Server.Types

router :: [Text] -> StdMethod -> Params -> Response
router []               GET  _      = App.homePath
router ["urls"]         GET  _      = App.indexPath
router ["urls"]         POST params = App.createPath params
router ["urls", urlId]  GET  _      = App.showPath $ read urlId
router _                _    _      = error404

error400 :: Response
error400 = App.error500Path

error404 :: Response
error404 = App.error404Path
