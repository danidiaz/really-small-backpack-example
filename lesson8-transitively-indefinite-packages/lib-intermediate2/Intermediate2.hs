module Intermediate2 (bazAsString) where

import Intermediate1 (barAsString)

bazAsString :: String
bazAsString = "****** " ++ barAsString ++ " plus baz"

