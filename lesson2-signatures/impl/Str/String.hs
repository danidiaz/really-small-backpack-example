module Str.String where

import qualified Data.List.Split

type Str = String

splitOn :: Char -> Str -> [Str]
splitOn c = Data.List.Split.splitOn [c]
