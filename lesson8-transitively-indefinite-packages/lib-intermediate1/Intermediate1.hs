{-# LANGUAGE TemplateHaskell #-}
module Intermediate1 (barAsString) where

import Core (fooAsString,makeIdFunc)

-- remove this and it compiles
$(makeIdFunc)

barAsString :: String
barAsString = "****** " ++ fooAsString ++ " plus bar"

