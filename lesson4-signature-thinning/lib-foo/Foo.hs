module Foo where

import Foo.Siggy (fooRequiresThis)

foo :: IO ()
foo = print fooRequiresThis
