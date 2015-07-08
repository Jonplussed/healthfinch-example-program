{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Server.Controller
( indexAction
, createAction
, showAction
) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Hasql.Postgres (Postgres)
import Histogram (Histogram, urlTextHistogram)
import Network.URI (URI, parseAbsoluteURI)
import Server.Database (query)
import Server.Response (html, redirectTo, error404)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Hasql as Db
import qualified Network.Wai as Wai
import qualified Server.Error as Err
import qualified Server.Views as View
import qualified Server.Model as Mod

import Server.Types

-- constants

urlParam, urlPath :: C8.ByteString
urlParam = "url"
urlPath = "/url"

-- actions

indexAction :: Params -> ServerM Wai.Response
indexAction _ = return $ html View.indexPage

createAction :: Params -> ServerM Wai.Response
createAction params = do
    url <- urlFromParams params
    alreadyExists <- query $ Mod.doesHistogramExist url
    if alreadyExists
      then return ()
      else generateHistogram url
    return $ redirectTo urlPath params

showAction :: Params -> ServerM Wai.Response
showAction params = return $ html View.indexPage

-- helper functions

urlFromParams :: Params -> ServerM URI
urlFromParams params =
    case Map.lookup urlParam params of
      Just bytes -> case parseAbsoluteURI (C8.unpack bytes) of
        Just url -> return url
        _ -> lift . throwError $ InvalidParam urlParam
      _ -> lift . throwError $ MissingParam urlParam

generateHistogram :: URI -> ServerM ()
generateHistogram url = do
    histogram <- liftIO $ urlTextHistogram url
    case histogram of
      Just h -> query $ Mod.createHistogram url h
      _ -> lift $ throwError LynxError
