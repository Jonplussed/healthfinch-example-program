{-# LANGUAGE OverloadedStrings #-}

module Server.Controller
( indexAction
, createAction
, showAction
) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Histogram (urlTextHistogram)
import Network.URI (parseAbsoluteURI)
import Server.Response (html, redirectTo, error404)

import qualified Data.Map as Map
import qualified Network.Wai as Wai
import qualified Server.Error as App
import qualified Server.Views as App

import Server.Types

indexAction :: Params -> ServerM Wai.Response
indexAction _ = return $ html App.indexPage

createAction :: Params -> ServerM Wai.Response
createAction params = return $ html App.indexPage
    -- case Map.lookup "url" params of
    --   Just url -> case parseAbsoluteURI url of
    --     Just valid -> do
    --       histogram <- liftIO $ urlTextHistogram url
    --       case histogram of
    --         Just h -> return . html . App.showView url $ Map.toList h
    --         _ -> throwError CannotCreateHistogram
    --     _ -> throwError InvalidParam "url"
    --   _ -> throwError $ MissingParam "url"

showAction :: UrlId -> Params -> ServerM Wai.Response
showAction urlId _ = return $ html App.indexPage
