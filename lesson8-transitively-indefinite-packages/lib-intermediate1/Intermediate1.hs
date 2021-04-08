{-# LANGUAGE TemplateHaskell, ImportQualifiedPost #-}
module Intermediate1 (barAsString) where

import Core (fooAsString)

import Intermediate1.Splices qualified

-- remove this and it compiles
$(Intermediate1.Splices.makeIdFunc)

barAsString :: String
barAsString = "****** " ++ fooAsString ++ " plus bar"

