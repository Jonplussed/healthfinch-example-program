{-# LANGUAGE OverloadedStrings #-}

module Server.Views
( indexPage
, showPage
, error404Page
, error500Page
) where

import Control.Monad (forM_)
import Network.URI (URI)
import Data.ByteString (ByteString)
import Text.Blaze.Html (Html)

import qualified Text.Blaze.Html5 as Html

import Server.Types

indexPage :: Html
indexPage = Html.h1 $ Html.toHtml ("success!" :: ByteString)

showPage :: URI -> [(ByteString, Int)] -> Html
showPage url words = do
    Html.table $ do
      forM_ words $ \(word, count) -> do
        Html.tr $ do
          Html.td $ Html.toHtml word
          Html.td $ Html.toHtml count

error404Page :: Html
error404Page = undefined

error500Page :: Html
error500Page = undefined

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
