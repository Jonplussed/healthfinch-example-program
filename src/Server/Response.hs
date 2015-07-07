{-# LANGUAGE OverloadedStrings #-}

module Server.Response
( html
, redirectTo
, error404
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import qualified Data.Map as Map
import qualified Network.HTTP.Types as Net
import qualified Network.Wai as Wai

import Server.Types

html :: Html -> Wai.Response
html contents = Wai.responseBuilder status headers body
  where
    status = Net.status200
    headers = [htmlContentType]
    body = renderHtmlBuilder contents

redirectTo :: ByteString -> Params -> Wai.Response
redirectTo url params = Wai.responseBuilder status headers body
  where
    status = Net.status303
    query = Net.renderSimpleQuery True $ Map.toList params
    headers = [(Net.hLocation, url `mappend` query)]
    body = mempty

error404 :: Html -> Wai.Response
error404 contents = Wai.responseBuilder status headers body
  where
    status = Net.status404
    headers = [htmlContentType]
    body = renderHtmlBuilder contents

-- helper functions

htmlContentType :: Net.Header
htmlContentType = (Net.hContentType, "text/html")
