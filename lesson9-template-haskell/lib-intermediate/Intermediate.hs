{-# LANGUAGE TemplateHaskell, ImportQualifiedPost #-}
module Intermediate (barAsString, myIdFunc) where

import Core qualified 

import Core.TH qualified -- the splice code comes from an as-yet indefinite package.
import Intermediate.TH qualified -- the splice code comes from a non-indefinite package.

-- this works, no problem
$(Intermediate.TH.makeIdFunc ''Core.A)

-- commenting out the previous splice and enabling this one results in an obscure error
-- $(Core.TH.makeIdFunc ''Core.A)


barAsString :: String
barAsString = "****** " ++ Core.fooAsString ++ " plus bar"

