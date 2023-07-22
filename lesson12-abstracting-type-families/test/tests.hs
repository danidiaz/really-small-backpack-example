{-# LANGUAGE OverloadedStrings #-} 
module Main where

import Lesson12.User
import Lesson12.Mystery
import Test.Tasty
import Test.Tasty.HUnit
import Data.Functor.Identity

user1 :: User NormalMode
user1 = User { name = "foo", age= 50}

user2 :: User MysteryMode
user2 = User { name = "foo", age= () }

tests =
  testGroup
    "All"
    [ ]

main :: IO ()
main = defaultMain tests
