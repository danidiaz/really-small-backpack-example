module Core (fooAsString,A) where

import Core.SomeSig (foo,A)

-- uses the abstract value from the signature and converts it to String
fooAsString :: String
fooAsString = show foo

