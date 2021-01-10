# lesson 7 - module identity

As we have seen, modules in "indefinite" packages can import module
signatures. Consumers of an indefinite package can "instantiate" it by
depending on both the indefinite package and a suitable implementation of its
signatures—possibly requiring some `mixins:` shenanigans to ensure a happy
match.

Suppose we go wild in the `mixins:` section, renaming modules and module signatures,
making multiple copies of both. We might end up with two or more "instantiated"
modules which have different names but which, if we trace back the chains of
module and module signature renamings, are actually produced by the *exact same
combination* of  module signatures and implementation modules.

The [cabal file](./package.cabal) for this lesson offers a simple example.
In the executable, the `Pair1` and `Pair2` modules are produced by
the same combination of module signature and implementation module.

The big question is: *are the types and functions in `Pair1` and `Pair2`
mutually compatible?* Those modules define pair types along with functions for building
and extracting the components of a pair. What [happens](./Main.hs) if we build
a pair from `Pair1` and try to extract its first component using a function
from `Pair2`?

    main :: IO ()
    main = print $ Pair1.pairFst $ Pair2.buildPair 1 2

Different module systems (Backpack, or [ML](https://people.mpi-sws.org/~dreyer/thesis/main.pdf)'s) have different answers to this question:

- If they make the types in `Pair1` and `Pair2` compatible, they are called *applicative module systems* (no relation with the `Applicative` typeclass).

- If they make the types in `Pair1` and `Pair2` distinct, they are called *generative module systems*.

So, which type of module system is Backpack?  If we try to compile and run with

    cabal run lesson7

We'll see that it works without problems. Backpack is applicative.

## See also

- [Understanding and Evolving the ML Module System](https://people.mpi-sws.org/~dreyer/thesis/main.pdf) by Mark Dreyer. Section 1.2.6 "The importance of generativity".

