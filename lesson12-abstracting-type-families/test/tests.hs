module Main where

import Lesson12.User
import Test.Tasty
import Test.Tasty.HUnit
import Data.Functor.Identity

tests =
  testGroup
    "All"
    [ ]

main :: IO ()
main = defaultMain tests
