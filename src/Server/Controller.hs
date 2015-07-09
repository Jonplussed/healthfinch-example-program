{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Server.Controller
( indexAction
, createAction
, showAction
) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Histogram (urlTextHistogram)
import Network.URI (URI)
import Server.Database (query)
import Server.Response (html, redirectTo)

import qualified Data.Map as Map
import qualified Network.Wai as Wai
import qualified Server.Views as View
import qualified Server.Model.Histogram as Hist
import qualified Server.Model.Url as Url

import Server.Types

-- actions

indexAction :: Params -> ServerM Wai.Response
indexAction _ = return $ html View.indexPage

createAction :: Params -> ServerM Wai.Response
createAction params = do
    url <- urlFromParams params
    alreadyExists <- query $ Hist.doesHistogramExist url
    if alreadyExists
      then return ()
      else generateHistogram url
    return $ redirectTo "/histogram" params

showAction :: Params -> ServerM Wai.Response
showAction params = do
    url <- urlFromParams params
    rows <- query $ Hist.listHistogramWords url
    return . html $ View.showPage url rows

-- helper functions

urlFromParams :: Params -> ServerM URI
urlFromParams params =
    case parsedUrl of
      Just url -> return url
      _ -> lift . throwError $ InvalidParam "url"
  where
    parsedUrl = Map.lookup "url" params >>= Url.parseUrl

generateHistogram :: URI -> ServerM ()
generateHistogram url = do
    histogram <- liftIO $ urlTextHistogram url
    case histogram of
      Just h -> query $ Hist.createHistogram url h
      _ -> lift $ throwError LynxError
