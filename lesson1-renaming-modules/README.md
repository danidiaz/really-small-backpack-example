# lesson 1 - renaming modules

In recent versions of Cabal, there is a new
[`mixins:`](https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-mixins)
field that allows us to make modified local "copies" of libraries mentioned in
the
[`build-depends:`](https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-build-depends)
field.

These "copies" differ from the originals in that their modules might have been
renamed (module signatures might be renamed as well, but let's leave
that for the next lesson).

In the example, we make two copies of the internal library `foo`: one in which
module `Foo` has been renamed to `Bar` and another in which `Foo` has been
renamed to `Baz`:

```
   mixins:
           foo (Foo as Bar),
           foo (Foo as Baz)
```

These modules are both imported by the module `Lesson1`:

```
import qualified Bar
import qualified Baz
```

Compile in this folder with the command:

```
cabal build
```
Open a repl in the main library with

```
cabal repl lib:lesson1-renaming-modules
```
Open a repl in the convenience library with

```
cabal repl lib:foo
```

## What happens to modules that are not renamed?

In the example code, the library `foo` has an additional `Foo.Extra` module
that we did *not* rename. What happens if we try to import it from `Lesson1`?

```
import qualified Foo.Extra
```

We get an error! How come?

```
lib/Lesson1.hs:5:1: error:
    Could not load module ‘Foo.Extra’
```

This is an important detail about how `mixins:` works. If we make a local copy
of a library and rename some modules, *all the other modules from the original
library that aren't explicily mentioned in the `mixins:` entry become hidden*.
The original form of the dependency is superseded by the altered copies.

What we can do? Mention that module in `mixins:`, too:

```
   mixins:
           foo (Foo as Bar),
           foo (Foo as Baz, Foo.Extra as Baz.Extra)
```

As we see, when making a "copy" of a library we can rename more than one module
by separating the renamings with commas. 

And now the previously hidden module can be imported:

```
import qualified Baz.Extra
```


## Renaming modules from external packages

We can rename modules from external package dependencies, not only modules of
internal libraries.

For example, we can take the package `bytestring` and rename the modules
`Data.ByteString` and `Data.ByteString.Lazy` to `Bytes` and `Bytes.Lazy`
respectively:

```
   mixins:
           bytestring (Data.ByteString as Bytes, Data.ByteString.Lazy as Bytes.Lazy)
```

We wouldn't be able to import `Data.ByteString.Builder` in our code however,
because it's not mentioned int the `mixins:` entry!

## See also

- This [Stack Overflow
  answer](https://stackoverflow.com/questions/47110907/what-should-i-do-if-two-modules-share-the-same-name/47111418#47111418)
  about using 'mixins:' to avoid name collisions between modules of different
  packages.

  `mixins:` is a more principled alternative to the `-XPackageImports` extension.

