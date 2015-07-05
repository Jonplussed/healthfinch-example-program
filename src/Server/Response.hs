{-# LANGUAGE OverloadedStrings #-}

module Server.Response
( html
, redirect
, error404
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import qualified Network.HTTP.Types as Net
import qualified Network.Wai as Wai

html :: Html -> Wai.Response
html contents = Wai.responseBuilder status headers body
  where
    status = Net.status200
    headers = [htmlContentType]
    body = renderHtmlBuilder contents

redirect :: ByteString -> Wai.Response
redirect url = Wai.responseBuilder status headers body
  where
    status = Net.status303
    headers = [(Net.hLocation, url)]
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
