module Lesson11.Foo (
    -- The internals of Foo are *not* exported, the only way 
    -- to pry into them is through the Inspectable instance.
    Foo, 
    zeroFoo, 
    succFoo) where 

import Lesson11.Inspectable
import Lesson11.Mystery

data N = Z | S N

data Foo = Foo N

instance Inspectable Foo where 
    inspect (Foo n) = wrappedInMystery $ inspectN n
      where
        inspectN Z = ""
        inspectN (S x) = '*' : inspectN x

zeroFoo :: Foo
zeroFoo = Foo Z

succFoo :: Foo -> Foo
succFoo (Foo n) = Foo (S n)

