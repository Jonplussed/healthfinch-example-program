{-# LANGUAGE OverloadedStrings #-}

module Server.Views
( homeView
, indexView
, showView
, error404View
, error500View
) where

import Control.Monad (forM_)
import Network.URI (URI)
import Data.ByteString (ByteString)
import Text.Blaze.Html (Html)

import qualified Text.Blaze.Html5 as Html

import Server.Types

homeView :: Html
homeView = Html.h1 $ Html.toHtml ("success!" :: ByteString)

indexView :: Html
indexView = undefined

showView :: URI -> [(ByteString, Int)] -> Html
showView url words = do
    Html.table $ do
      forM_ words $ \(word, count) -> do
        Html.tr $ do
          Html.td $ Html.toHtml word
          Html.td $ Html.toHtml count

error404View :: Html
error404View = undefined

error500View :: Html
error500View = undefined

-- helper functions

template :: Html -> Html
template contents = do
    Html.docTypeHtml $ do
      Html.head $ do
        Html.title "URL Text Histogram"
      Html.body $ do
        Html.nav $ do
          Html.ul $ do
            Html.li "hello"

-- stylesheet :: Text -> Html
-- stylesheet url = Html.link ! rel "stylesheet" ! type "text/css" ! href url
