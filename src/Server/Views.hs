module Server.Views
( homeView
, indexView
, showView
, error404View
, error500View
) where

import Text.Blaze.Html (Html)

import qualified Text.Blaze.Html5 as Html

homeView :: Html
homeView = Html.h1 $ Html.toHtml "success!"

indexView :: Html
indexView = undefined

showView :: Html
showView = undefined

error404View :: Html
error404View = undefined

error500View :: Html
error500View = undefined
