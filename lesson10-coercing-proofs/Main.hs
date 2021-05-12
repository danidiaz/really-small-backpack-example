module Main where

import Lesson10

main :: IO ()
main = do
    print $ reverse $ 'a' :> 'b' :> 'c' :> Nil


