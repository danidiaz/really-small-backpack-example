# lesson 2 - signatures

In this lesson we will use Backpack to abstract over concrete module
implementations.

We have a ridiculously simplistic "template engine" defined in the
[`Lesson2`](./lib/Lesson2.hs) module. It lets us "compile" a string into a
`Template` and then render the template with different arguments.

However, we don't want to depend on any particular string implementation. We
want it to be easily adaptable for many string types, without cluttering the
API with new type parameters or typeclass constraints. How to do this?

We have defined an [abstract
signature](https://wiki.haskell.org/Module_signature) [`Str`](./lib/Str.hsig)
with declares only those operations that are needed for implementing the
templating functionality. The signature is listed in the ["signatures:"](https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-library-signatures)
section of the Cabal file. `Lesson2` imports that signature instead of any
concrete implementation module. 

This means that the library has a "hole" with a "shape" defined by `Str`. 

This package also defines an executable named ["lesson2"](./Main.hs) that wants to use the
library. Use it with two different string types, in fact. How to do this?

First we must find or define modules that give concrete implementation to the
signature. We do this in the `impl-string` and `impl-text` internal
libraries. We could also use compatible modules from completely different
packages.

Notice that the [`Str.String`](./impl/Str/String.hs) and
[`Str.Text`](./impl/Str/Text.hs) modules don't know *anything* about the `Str`
signature. They just happen to be compatible with it.

Now for the mixin magic. In a previous lesson we used `mixins:` to create local
copies of libraries where the modules had been renamed. Turns out that
`mixins:` can also rename the *signatures* required by a library! This can be
difficult to wrap your head around at first.

In the cabal file we see:

```
    mixins:
        lesson2-signatures (Lesson2 as Lesson2.String) requires (Str as Str.String), 
        lesson2-signatures (Lesson2 as Lesson2.Text) requires (Str as Str.Text) 
```

We are making two new versions of `Lesson2`. Besides giving them different
names, we are using the `requires` clause to *rename the required signature*,
making it overlap with the name of the appropriate implementation module.
Mixing matching is triggered by a coincidence of module and signature names;
this is why the renaming machinery is important for Backpack.

And this is basically it. The `Main` module imports both `Lesson2.String` and
`Lesson2.Text` and makes use of them.

Incidentally, if we only had need for the `String` implementation, we could
have done something like this without ambiguity:

```
    mixins:
        lesson2-signatures (Lesson2) requires (Str as Str.String) 
```

mentioning `Lesson2` so as not to leave it hidden, but only renaming the `Str`
signature.


Compile in this folder with the command:

```
cabal build
```
Run the executable with:

```
cabal run lesson2
```

## An annoying limitation of implementation libraries

Could we have defined the implementation modules in the executable component
itself, without putting them in internal convenience libraries? That would have
been very convenient.

Sadly, the response is NO. This is a limitation of Backpack: the module that
fills a signature for a component must be defined in another component (be it a
convenience library or a completely separate package). 

## Other common gotchas 

- Having multiple entries in `mixins:` for the same dependency is *not*
  equivalent to having a single entry in `mixins:` that performs multiple
  renamings.

  In the first case we are creating *multiple* copies of the library, each of
  which can potentially have different implementations for their module
  signatures.

  In the second case we are creating a *single* copy of the library, in which
  several modules and/or signatures have been renamed.

  If you only want to instantiate an indefinite library in a single way,
  there's no reason to have multiple entries for the library in `mixins:`.

- Remember: renamings of module signatures always go in the `requires`.

- The library that contains the implementation module should *not* depend,
  directly or indirectly, on the module signature. 

## See also

- The full syntax allowed in the `mixins:` field can be found in the
  corresponding section of the [Cabal User
  Guide](https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-mixins).

- You might want to jump briefly to `lesson7` which deals with the issue of
  "module identity".

