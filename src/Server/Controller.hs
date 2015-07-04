module Server.Controller
( homePath
, indexPath
, createPath
, showPath
, error404Path
, error500Path
) where

import Server.Response (html, redirect, error404)

import qualified Network.Wai as Wai
import qualified Server.Views as App

import Server.Types

homePath :: Responder
homePath resp = undefined

indexPath :: Responder
indexPath resp = undefined

createPath :: Params -> Responder
createPath params resp = undefined

showPath :: UrlId -> Responder
showPath urlId resp = undefined

error404Path :: Responder
error404Path resp = undefined

error500Path :: Responder
error500Path resp = undefined
