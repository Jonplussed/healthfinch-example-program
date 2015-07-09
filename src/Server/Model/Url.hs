{-# LANGUAGE OverloadedStrings #-}

module Server.Model.Url
( parseUrl
) where

import Network.URI (URI, parseAbsoluteURI)

import qualified Data.ByteString.Char8 as C8

parseUrl :: C8.ByteString -> Maybe URI
parseUrl = parseAbsoluteURI . C8.unpack . prependHost

-- helpers

prependHost :: C8.ByteString -> C8.ByteString
prependHost url =
    if C8.isPrefixOf "http" url
      then url
      else "http://" `mappend` url
