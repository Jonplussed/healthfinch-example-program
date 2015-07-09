{-# LANGUAGE OverloadedStrings #-}

module Server.Response
( html
, redirectTo
, error404
, error500
) where

import Data.ByteString (ByteString)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import qualified Data.Map as Map
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai

import Server.Types

html :: Html -> Wai.Response
html contents = Wai.responseBuilder status headers body
  where
    status = Http.status200
    headers = [htmlContentType]
    body = renderHtmlBuilder contents

redirectTo :: ByteString -> Params -> Wai.Response
redirectTo url params = Wai.responseBuilder status headers body
  where
    status = Http.status303
    query = Http.renderSimpleQuery True $ Map.toList params
    headers = [(Http.hLocation, url `mappend` query)]
    body = mempty

error404 :: Html -> Wai.Response
error404 contents = Wai.responseBuilder status headers body
  where
    status = Http.status404
    headers = [htmlContentType]
    body = renderHtmlBuilder contents

error500 :: Html -> Wai.Response
error500 contents = Wai.responseBuilder status headers body
  where
    status = Http.status500
    headers = [htmlContentType]
    body = renderHtmlBuilder contents

-- helper functions

htmlContentType :: Http.Header
htmlContentType = (Http.hContentType, "text/html")
