{-# LANGUAGE OverloadedStrings #-}

module Server.Model.Url
( parseUrl
) where

import Network.URI (URI, parseAbsoluteURI)

import qualified Data.ByteString.Char8 as C8

parseUrl :: C8.ByteString -> Maybe URI
parseUrl = parseAbsoluteURI . C8.unpack
