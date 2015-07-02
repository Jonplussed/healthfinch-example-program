module Main (main) where

import Histogram.Parser (urlTextHist)

main :: IO ()
main = urlTextHist "stellenbauchery.com" >>= putStrLn . show
