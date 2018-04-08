module Str.String where

import qualified Data.List.Split

-- type synonym to make the names match
type Str = String

-- the names and the types must match with the signature
splitOn :: Char -> Str -> [Str]
splitOn c = Data.List.Split.splitOn [c]

-- implementation modules can have other functions and types defined
blah :: String
blah = "blah"
