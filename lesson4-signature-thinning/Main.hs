module Main where

import Foo (foo)
import Bar (bar)

main :: IO ()
main = do
    foo
    bar
    return ()
