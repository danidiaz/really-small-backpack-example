What happens if in one component we import two libraries "with holes" and the
"holes" (that is, the signatures) happen to have the same name?

When we want to import two modules that have the same name (say, [Crypto.Random
from cryptonite and Crypto.Random from
crypto-api](https://stackoverflow.com/questions/47110907/what-should-i-do-if-two-modules-share-the-same-name))
that is a problem. One solution is to use the
[-XPackageImports](http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=packageimports#extension-PackageImports)
extension. This is a bit ugly because forces our module code to be aware of
package names. Another (more elegant, to my mind) solution is to use the module
renaming capabilities of Backpack that we saw earlier.

Signatures behave quite differently from modules in this respect. As long as
the signatures don't have mutually incompatible definitions (say, functions
with the same name but incompatible types) the signatures are simply *merged*.

The code in this chapter provide an example of this. We have two "libraries
with holes" [foo](lib-foo) and [bar](lib-bar). Both of them—independently—
declare a signature **Siggy**. The executable imports both libraries and
provies an [implementation](lib-impl/Siggy.hs) for the signatures.

Looking at the signatures, we notice that the one from **bar** refers to and
additional **someOtherVal** value. And that is not all: the type **T** is *more
defined* in the signature froom **bar**. Backpack is smart enough to check that
the signatures are compatible and merges them. Of course, the implementation
module must satisfy the merged signature.

(By the way, how come there is no `mixin` section in the
[`package.cabal`](package.cabal) file? The reason is that I chose the names of
the signatures and of the implementation package so that they mach from the
beginning, so no renaming is necessary.)

Compile in this folder with the command:

```
cabal new-build
```
Run the executable with:

```
cabal new-run 
```
