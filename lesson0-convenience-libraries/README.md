This first lesson is not really about Backpack, but about the "convenience
libraries" feature of Cabal.

Basically, you can add additional named library sections in your Cabal file,
besides the default library section.

These named "convenience libraries" are not seen outside the package, but the
main library, the executables and the testsuites can depend on them, which can
be very useful for sharing code. 

In the example, `lib-foo` is a named convenience library, providing the package `Foo`.

Compile in this folder with the command:

```
cabal new-build
```

Open a repl in the main library with

```
cabal new-repl lib:lesson0-convenience-libraries
```

Open a repl in the convenience library with

```
cabal new-repl lib:foo
```
