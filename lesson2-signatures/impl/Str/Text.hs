module Str.Text where

import qualified Data.Text

-- type synonym to make the names match
type Str = Data.Text.Text

-- the names and the types must match with the signature
splitOn :: Char -> Str -> [Str]
splitOn c = Data.Text.splitOn (Data.Text.singleton c)
