module Lesson11.UsesFoo where 

import Lesson11.Foo

stuffThatUsesFoo :: Foo
stuffThatUsesFoo = succFoo (succFoo zeroFoo)

-- We could also try to inspect some Foo but, because this indefinite package
-- doesn't know anything about Mystery, we wouldn't be able to do *anything*
-- with the resulting Mystery String
--
