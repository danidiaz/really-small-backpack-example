# lesson 0 - convenience libraries

This first lesson is not really about Backpack, but about the ["internal
libraries"](https://www.haskell.org/cabal/users-guide/developing-packages.html?highlight=library#library)
feature of Cabal.

Basically, you can add additional named library sections in your Cabal file,
besides the default library section.

These named "convenience libraries" are not seen outside the package, but the
main library, the executables and the testsuites can depend on them, which can
be very useful for sharing code. 

In the example, `foo` is a named convenience library, providing the module `Foo`.

Compile in this folder with the command:

```
cabal build
```

Open a repl in the main library with

```
cabal repl lib:lesson0-convenience-libraries
```

Open a repl in the convenience library with

```
cabal repl lib:foo
```
