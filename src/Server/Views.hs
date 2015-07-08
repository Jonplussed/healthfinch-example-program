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

indexPage :: Html
indexPage = template $ do
    Html.h1 "URL text histogram"
    Html.h3 "Generate a frequency list of every word at the given URL"
    Html.form ! Attr.action "/histogram" ! Attr.method "post" $ do
      Html.input ! Attr.type_ "url"
      Html.input ! Attr.type_ "submit"

showPage :: URI -> [(Text, Int)] -> Html
showPage url words = template $
    Html.table $
      forM_ words $ \(word, count) ->
        Html.tr $ do
          Html.td $ Html.toHtml word
          Html.td $ Html.toHtml count

error404Page :: Html
error404Page = undefined

error500Page :: Html
error500Page = undefined

-- helper functions

template :: Html -> Html
template contents =
    Html.docTypeHtml $ do
      Html.head $
        Html.title "URL Text Histogram"
      Html.body contents
