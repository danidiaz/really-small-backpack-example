module Main where

import Intermediate (barAsString, myIdFunc)

main :: IO ()
main = do
    print $ myIdFunc 3
    putStrLn $ barAsString
