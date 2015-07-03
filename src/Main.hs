module Main (main) where

import Histogram (urlTextHistogram)

main :: IO ()
main = urlTextHistogram "stellenbauchery.com" >>= putStrLn . show
