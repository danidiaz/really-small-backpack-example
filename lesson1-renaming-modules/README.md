# lesson 1 - renaming modules

In recent versions of Cabal, there is a new `mixins` section that allows us to
rename modules from our dependencies (it also allows us to rename signatures,
but let's leave that for the next lesson.)

We can make several renamed copies of the same module. In the example, we make
to copies of module `Foo` defined in the "foo" convenience library:

```
   mixins:
           foo (Foo as Bar),
           foo (Foo as Baz)
```

These are both imported by the module `Lesson1`:

```
import qualified Bar
import qualified Baz
```

Here's [a Stack Overflow
answer](https://stackoverflow.com/questions/47110907/what-should-i-do-if-two-modules-share-the-same-name/47111418#47111418)
about using "mixins" to rename modules.

We can rename modules from external packages as well. For example, we can
rename `Data.ByteString` from package `bytestring` to `Bytes`. That is left as
an exercise.

Compile in this folder with the command:

```
cabal v2-build
```
Open a repl in the main library with

```
cabal v2-repl lib:lesson1-renaming-modules
```
Open a repl in the convenience library with

```
cabal v2-repl lib:foo
```

