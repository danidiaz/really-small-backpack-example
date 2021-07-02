module Main where

import Lesson11.Foo
import Lesson11.UsesFoo
import Lesson11.Inspectable
import Test.Tasty
import Test.Tasty.HUnit
import Data.Functor.Identity

tests =
  testGroup
    "All"
    [ testCase "inspection" $
        assertEqual "" (runIdentity (inspect stuffThatUsesFoo)) "**"
    ]

main :: IO ()
main = defaultMain tests
