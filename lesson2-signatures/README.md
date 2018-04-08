In this lesson we will use Backpack to abstract over concrete module
implementations.

We have a ridiculously simplistic "template engine" defined in the
[`Lesson2`](./lib/Lesson2.hs) module. It lets us "compile" a string into a
`Template` and then render the template with different argument.

However, we don't want to depend on any particular string implementation. We
want it to be easily adaptable for many string types, without cluttering the
API with new type paramters or typeclass constraints. How to do this?

We have defined an [abstract
signature](https://wiki.haskell.org/Module_signature) [`Str`](./lib/Str.sig)
with declares only those operations that are needed for implementing the
templating functionality. `Lesson2` imports that signature instead of any
concrete implementation module.

```
cabal new-build
```

```
cabal new-exec lesson2
```
