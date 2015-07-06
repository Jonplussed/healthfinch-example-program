module Main (main) where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import Server (app)
import Server.Database (dbConfig, poolConfig)
import System.Environment (getArgs)

import qualified Hasql as Db
import qualified Hasql.Postgres as Db

main :: IO ()
main = do
    [port] <- liftIO getArgs
    withDb $ \db -> liftIO $ run (read port) (app db)
    return ()

withDb :: (Db.Pool Db.Postgres -> IO a) ->  IO a
withDb = bracket (Db.acquirePool dbConfig poolConfig) Db.releasePool
