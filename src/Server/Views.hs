{-# LANGUAGE OverloadedStrings #-}

module Server.Views
( indexPage
, showPage
, error404Page
, error500Page
) where

import Control.Monad (forM_)
import Network.URI (URI)
import Data.Text (Text)
import Text.Blaze ((!))
import Text.Blaze.Html (Html)

import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr

import Server.Types

indexPage :: [(Text, Text, Int, String)] -> Html
indexPage rows = template $ do
    Html.form ! Attr.action "/histogram" ! Attr.method "post" $ do
      Html.input ! Attr.type_ "url" ! Attr.name "url"
      Html.input ! Attr.type_ "submit"
    Html.table $ do
      Html.tr $ do
        Html.th "URL"
        Html.th "Most Frequent Word"
        Html.th "Count"
      forM_ rows $ \(url, word, freq, href) ->
        Html.tr $ do
          Html.td $
            Html.a ! Attr.href (Html.stringValue href) $ Html.toHtml url
          Html.td $ Html.toHtml word
          Html.td $ Html.toHtml freq

showPage :: URI -> [(Text, Int)] -> Html
showPage _ rows = template $
    Html.table $ do
      Html.tr $ do
        Html.th "Word"
        Html.th "Frequency"
      forM_ rows $ \(word, freq) ->
        Html.tr $ do
          Html.td $ Html.toHtml word
          Html.td $ Html.toHtml freq

error404Page :: Html
error404Page = template $
    Html.p "Error 404: Page Not found"

error500Page :: ServerError -> Html
error500Page err = template $
    Html.p . Html.toHtml $ "Error 500: " ++ show err

-- helper functions

template :: Html -> Html
template contents =
    Html.docTypeHtml $ do
      Html.head $
        Html.title "URL Text Histogram"
      Html.body $ do
        Html.h1 "URL Text Histogram"
        Html.h3 "See the frequency of every word at  given URL"
        contents
