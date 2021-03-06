# lesson 5 - abstract typeclasses

Over which entities, exactly, can we abstract in a signature?

In previous lessons we have abstracted over datatypes and standalone functions. Can we abstract over anything else?

Turns out that we can abstract over typeclasses, in a limited way.

(Note that, previously, we have required our abstract datatypes to implement
some externally defined typeclasses like `Monoid` or `Show`. But so far we
haven't written an abstract typeclass which will be given a concrete definition
later.)

A motivating example: imagine that we want to abstract over a `Map` container using Backpack.
Sometimes we will want an [ordered
map](http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Map-Strict.html),
sometimes a [hash
map](http://hackage.haskell.org/package/unordered-containers-0.2.10.0/docs/Data-HashMap-Strict.html).
The `lookup` functions for each map will have different constraints:
`Ord` in one case, `Eq` and
[`Hashable`](http://hackage.haskell.org/package/hashable-1.3.0.0/docs/Data-Hashable.html#t:Hashable)
in the other. What can we do to put both functions under the same abstract
definition in the signature?

In the signature [`Mappy`](./lib/Mappy.hsig) we can define an abstract typeclass like

    class Key k

Notice that we don't mention any methods or associated type families, just the class name and its parameters.

Now our abstract `lookup` and `fromList` functions can be written like

    lookup :: (Eq k,Key k) => k -> Map k a -> Maybe a

    fromList :: (Eq k,Key k) => [(k, v)] -> Map k v

Later, in the implementations ([`MappyOrdered`](./impl/MappyOrdered.hs) and [`MappyHash`](./impl/MappyHash.hs)) we have these mappings from `Key` to actual typeclasses:

    type Key = Ord

and

    type Key = Hashable

Such definitions require the
[ConstraintKinds](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=constraintkinds#extension-ConstraintKinds)
extension.

The [`Main`](./Main.hs) module instantiates the library both ways and creates two maps
with `Int` keys. `Int` has `Eq`, `Ord` and `Hashable` instances, so both
instantiations compile.

Compile in this folder with the command:

```
cabal build
```
Run the executable with:

```
cabal run lesson5
```

## What about composite constraints?

At one point I tried, in the `MappyHash` implementation module, to make the
abstract class `Key` stand for a combination of constraints, something like 

    type Key k = (Eq k, Hashable k) 

That would have simplified the signatures of `lookup` and `fromList` in
`Mappy`, as we wouldn't need to put the `Eq` constraints there. But I couldn't
make it work.

The reason is that Backpack requires type synonyms which define implementations
to be "eta-reduced". That is, you can write `type Key = Ord` but not `type Key
k = Ord k`.

Maybe some kind of [constraint synonym trick](https://kcsongor.github.io/opaque-constraint-synonyms/) could help here?


## Abstract typeclasses and concrete types

Our [`Lesson5`](./lib/Lesson5.hs) module is quite trivial and not very useful. It merely re-exports `Mappy`. But what
if we wanted to add some logic there; create a map with `Int` keys just like
we did in `Main`, but still keeping the map abstract? Something like

    module Lesson5 (module Mappy,doStuff) where

    import Mappy

    doStuff :: IO ()
    doStuff = do
        print $ Mappy.lookup (1::Int) (Mappy.fromList [(1,True),(2,False)])

alas, it doesn't compile:

    lib/Lesson5.hs:10:13: error:
        • No instance for (Key Int) arising from a use of ‘{Mappy.lookup}’

Why? Well, consider that—unlike [`Main`](./Main.hs)—[`Lesson5`](./lib/Lesson5.hs) lives in an indefinite
library. It doesn't know anything about possible implementations of
`Mappy`—check its dependencies in the [`.cabal`](./package.cabal) file! Therefore, it can't be
sure that `Int` has the required `Key` instance. That typeclass could be
anything, after all!

What can we do? Turns out that we can explicitly require the instance in
`Mappy`:

    class Key k
    instance Key Int -- I don't know which class "Key" is, but I want Int to have an instance!

and now it compiles.

So: we can define abstract typeclasses, and add them as constraints to methods
of a module signature. We can also demand that certain known types have
instances of the typeclass.

## Other stuff that can go into module signatures

The GHC User Guide has a [detailed
description](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/separate_compilation.html#module-signatures)
of what elements can go into a module signature. Definitely worth a read if you plan to use Backpack.

Some interesing tidbits:

- Besides the *abstract* classes we have seen in this lesson, we can also put
  *concrete* class declarations, with superclasses and methods! Kind of surprising.

- Same for datatypes: we can write *concrete* datatype definitions, with constructors! Also kind of surprising.

- We can also put closed type families, both abstract and concrete.

