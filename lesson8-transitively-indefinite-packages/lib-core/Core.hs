module Core (fooAsString) where

import Core.SomeSig (foo)

-- uses the abstract value from the signature and converts it to String
fooAsString :: String
fooAsString = show foo

