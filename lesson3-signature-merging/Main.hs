module Main where

import Foo (printFooVal)
import Bar (printBarVal)

main :: IO ()
main = do
    printFooVal
    printBarVal
    return ()
