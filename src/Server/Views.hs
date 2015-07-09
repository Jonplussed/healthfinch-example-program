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
    Html.p "See the frequency of every word at the given URL"
    Html.form ! Attr.action "/histogram" ! Attr.method "post" $ do
      Html.input ! Attr.type_ "url" ! Attr.name "url"
      Html.input ! Attr.type_ "submit"

    Html.h3 "Previous URLs entered:"
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
showPage url rows = template $ do
    Html.p $ do
      Html.toHtml $ ("Top 10 words used in " :: String)
      Html.a ! Attr.href (Html.stringValue urlStr) $ Html.toHtml urlStr
      Html.toHtml $ (". " :: String)
      Html.a ! Attr.href "/" $ "Try another URL"
    Html.table $ do
      Html.tr $ do
        Html.th "Word"
        Html.th "Frequency"
      forM_ rows $ \(word, freq) ->
        Html.tr $ do
          Html.td $ Html.toHtml word
          Html.td $ Html.toHtml freq
  where
    urlStr = show url

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
      Html.head $ do
        Html.title "URL Text Histogram"
        Html.style $ Html.toHtml stylesheet
      Html.body $ do
        Html.h1 "URL Text Histogram"
        contents

-- NOTE: for *any* other application, I would add a static-file-serving WAI
-- middleware to serve assets. Because the "assets" are just a single
-- stylesheet, however, that doesn't seem worthwhile.
stylesheet :: String
stylesheet = unlines
    [ "body {"
    , "  font-size: 14px;"
    , "  font-family: Helvetica, sans-serif;"
    , "}"
    , ""
    , "table {"
    , "  border-collapse: collapse;"
    , "}"
    , ""
    , "table td, table th {"
    , "  text-align: left;"
    , "  padding: 5px;"
    , "}"
    , ""
    , "tr:nth-child(even) td, tr:nth-child(even) th {"
    , "  background-color: #EEE;"
    , "}"
    ]
