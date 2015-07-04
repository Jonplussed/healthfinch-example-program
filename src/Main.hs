module Main (main) where

import Network.Wai.Handler.Warp (run)
import Server (app)
import System.Environment (getArgs)

main :: IO ()
main = do
  [port] <- getArgs
  run (read port) app
