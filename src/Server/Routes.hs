{-# LANGUAGE OverloadedStrings #-}

module Server.Routes
( router
, error400
, error404
) where

import Data.Text (Text)
import Network.HTTP.Types (StdMethod (..))

import qualified Server.Controller as App

import Server.Types

router :: [Text] -> StdMethod -> Params -> Responder
router []               GET  _      = App.homePath
router ["urls"]         GET  _      = App.indexPath
router ["urls"]         POST params = App.createPath params
router ["urls", urlId]  GET  _      = App.showPath (fromPathSegment urlId)
router _                _    _      = error404

error400 :: Responder
error400 = App.error500Path

error404 :: Responder
error404 = App.error404Path
