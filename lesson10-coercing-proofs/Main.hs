module Main where

import Lesson10

import Prelude hiding (reverse)

main :: IO ()
main = do
    print $ reverse $ 'a' :> 'b' :> 'c' :> Nil


