module Server.Controller
( homePath
, indexPath
, createPath
, showPath
, error404Path
) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Histogram (urlTextHistogram)
import Network.URI (parseURI)
import Server.Response (html, redirect, error404)

import qualified Data.Map as Map
import qualified Network.Wai as Wai
import qualified Server.Views as App

import Server.Types

homePath :: Response
homePath = return $ html App.homeView

indexPath :: Response
indexPath = undefined

createPath :: Params -> Response
createPath params = undefined

showPath :: UrlId -> Response
showPath urlId = do
    histogram <- liftIO $ urlTextHistogram url
    case histogram of
      Just h -> return . html . App.showView url $ Map.toList h
      _ -> throwError CannotCreateHistogram
  where
    (Just url) = parseURI "http://stellenbauchery.com"

error404Path :: Wai.Response
error404Path = undefined
