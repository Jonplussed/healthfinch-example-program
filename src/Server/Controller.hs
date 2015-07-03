module Histogram.Controller
( homePath
, indexPath
, createPath
, showPath
, notFoundPath
, errorPath
) where

import qualified Network.WAI as Wai

import Server.Types

homePath :: Wai.Response
homePath _ = undefined

indexPath :: Wai.Response
indexPath _ = undefined

createPath :: Params -> Wai.Response
createPath _ = undefined

showPath :: UrlId -> Wai.Response
showPath _ = undefined

error404Path :: Wai.Response
error404Path = undefined

error500Path :: Wai.Response
error500Path = undefined
