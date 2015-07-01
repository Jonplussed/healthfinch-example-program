module Main (main) where

import Words.Parser (tabUrlText)

main :: IO ()
main = tabUrlText "stellenbauchery.com" >>= putStrLn . show
