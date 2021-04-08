module Core.SomeImpl (foo,A) where

foo :: Int
foo = 5 -- here we have a concrete value, unlike in the .hsig file

type A = Int
