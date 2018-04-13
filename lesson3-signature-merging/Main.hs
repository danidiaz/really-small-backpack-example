module Main where

import Foo
import Bar

main :: IO ()
main = do
    printFooVal
    printBarVal
    return ()
