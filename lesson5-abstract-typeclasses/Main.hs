module Main where


import qualified Lesson5Ordered
import qualified Lesson5Hash

main :: IO ()
main = do
    Lesson5Ordered.doStuff
    Lesson5Hash.doStuff
