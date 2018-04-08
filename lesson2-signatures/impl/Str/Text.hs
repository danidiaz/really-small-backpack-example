module Str.Text where

import qualified Data.Text

type Str = Data.Text.Text

splitOn :: Char -> Str -> [Str]
splitOn c = Data.Text.splitOn (Data.Text.singleton c)
