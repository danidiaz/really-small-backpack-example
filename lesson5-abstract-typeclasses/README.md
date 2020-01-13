# lesson 5 - abstract typeclasses

Over which entities, exactly, can be abstract in a signature?

In previous lessons we have abstracted over datatypes and standalone functions. Can we abstract over anything else?

Turns out that we can abstract over typeclasses, in a limited way.

(Note that, previously, we have required our abstract datatypes to implement
some externally defined typeclasses like `Monoid` or `Show`. But so far we
haven't written an abstract typeclass which will be given a concrete
"implementation" later.)

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

In the signature (`Mappy`) we can define an abstract typeclass like

    class Key k

Notice that we don't mention any methods or associated type families, just the class name and its parameters.

Now our abstract `lookup` and `fromList` functions can be written like

    lookup :: (Eq k,Key k) => k -> Map k a -> Maybe a

    fromList :: (Eq k,Key k) => [(k, v)] -> Map k v

Later, in the implementations (`MappyOrd` and `MappyHash`) we have these mappings from `Key` to actual typeclasses:

    type Key = Ord

and

    type Key = Hashable

Such definitions require the
[ConstraintKinds](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=constraintkinds#extension-ConstraintKinds)
extension.

The `Main` module instantiates the library both ways and creates two maps
with `Int` keys. `Int` has `Eq`, `Ord` and `Hashable` instances, so both
instantiations compile.

Compile in this folder with the command:

```
cabal v2-build
```
Run the executable with:

```
cabal v2-run lesson5
```

---

**Note**: 

At one point I tried to make the abstract class `Key` to stand for a
combination of constraints, something like 

    type Key k = (Eq k, Hashable k) 

That would have simplified the signature of `lookup`, but I couldn't make it work.

