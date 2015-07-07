{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Server.Controller
( indexAction
, createAction
, showAction
) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Hasql.Postgres (Postgres)
import Histogram (urlTextHistogram)
import Network.URI (parseAbsoluteURI)
import Server.Response (html, redirectTo, error404)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Hasql as Db
import qualified Network.Wai as Wai
import qualified Server.Error as Err
import qualified Server.Views as View
import qualified Server.Model as Mod

import Server.Types

indexAction :: Params -> ServerM Wai.Response
indexAction _ = return $ html View.indexPage

createAction :: Params -> ServerM Wai.Response
createAction params = do
    url <- urlFromParams params
    rows <- query $ Mod.listWordsForUrl url
    return $ redirectTo "/url" params

showAction :: Params -> ServerM Wai.Response
showAction params = return $ html View.indexPage

-- helper functions

urlFromParams :: Params -> ServerM Text.Text
urlFromParams params =
    case Map.lookup pname params of
      Just bytes -> case parseAbsoluteURI (C8.unpack bytes) of
        Just url -> return . Text.pack $ show url
        _ -> lift . throwError $ InvalidParam pname
      _ -> lift . throwError $ MissingParam pname
  where
    pname = "url"

query :: (forall s. Db.Tx Postgres s a) -> ServerM a
query = Db.tx (Just (txLevel, writeable))
  where
    txLevel = Db.ReadCommitted
    writeable = Just True
