module Server.Error
( error404
, error500
) where

import qualified Network.Wai as Wai
import qualified Server.Response as Resp
import qualified Server.Views as View

import Server.Types

error404 :: Wai.Response
error404 = Resp.error404 View.error404Page

error500 :: ServerError -> Wai.Response
error500 = Resp.error500 . View.error500Page
