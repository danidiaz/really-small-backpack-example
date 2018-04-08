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
templating functionality. The signature is declared in the "signatures:"
section of the Cabal file. `Lesson2` imports that signature instead of any
concrete implementation module. 

This means that the library has a "hole" with a "shape" defined by `Str`. 

This package also defines an executable named "lesson2" which wants to use the
library. Use it with two different string types, in fact. How to do this?

First we must find or define modules that give concrete implementation to the
signature. We do this in the "impl-string" and "impl-text" convenience
libraries. We could also use compatible modules from completely different
packages.

Notice that the [`Str.String`](./impl/Str/String.hs) and
[`Str.Text`](./impl/Str/Text.hs) modules don't know *anything* about the `Str`
signature. They just happen to be compatible with it.

Now for the mixin magic. In a previous lesson we used "mixins:" to create
renamed copies of modules. Turns out that "mixins:" can also rename the
*signatures* required by a module! This can be difficult to wrap your head
around at first.

In the cabal file we see:

```
    mixins:
        lesson2-signatures (Lesson2 as Lesson2.String) requires (Str as Str.String), 
        lesson2-signatures (Lesson2 as Lesson2.Text) requires (Str as Str.Text) 
```

We are making two different copies of `Lesson2`. Besides giving them names, we
are using the `requires` clause to *rename the required signature*, making it
overlap with the name of the appropiate implementation module. Mixing matching
is triggered by a coincidence of module and signature names; this is why the
renaming machinery is important for Backpack.

And this is basically it. The `Main` module imports both `Lesson2.String` and
`Lesson2.Text` and makes use of them.

Incidentally, if we only had need for the
`String` implementation, we could have done something like:

```
    mixins:
        lesson2-signatures requires (Str as Str.String) 
```

and have imported `Lesson2` without renaming it, because there wouldn't have
been ambiguity in that case.

One final thing: could have we defined the implementation modules in the
executable component itself, without putting them in convenience libraries? The
response is NO. This is a limitation of Backpack: the module that fills a
signature for a component must be defined in another component (be it a
convenience library or a completely separate package). 

Compile in this folder with the command:

```
cabal new-build
```
Run the executable with:

```
cabal new-run lesson2
```
