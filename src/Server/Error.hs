module Server.Error where

import qualified Network.Wai as Wai

import Server.Types

error404 :: Wai.Response
error404 = undefined

error500 :: Wai.Response
error500 = undefined
