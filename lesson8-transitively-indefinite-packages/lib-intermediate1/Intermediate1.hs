module Intermediate1 (barAsString) where

import Core (fooAsString)

barAsString :: String
barAsString = "****** " ++ fooAsString ++ " plus bar"

