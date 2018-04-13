module Siggy (C,T,someVal,someOtherVal) where

type C = [Int]

type T = [C]

someVal :: T
someVal = [[1]]

someOtherVal :: String
someOtherVal = "someOtherVal"
