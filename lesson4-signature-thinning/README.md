Signature thinning (not to confuse with signature merging) is an experimental
(?) feature inteded to facilitate the reuse of signatures.

Let's start with one idea: there can exist packages that only contain
signatures, and no implementation modules. Why are they useful?

So far in this course, every library component with "holes" declared a
signature tailored to their particular needs. In time, these ad-hoc signatures
will multiply throughout the ecosystem. Many of them will cover the same
concepts, say, the abstract interface of a string-like type. And yet, they are
likely to be incompatible, because each author will have used differente names
for the types and functions, or made sligtly different decisions on the types.
Furthermore, re-inventing already existing interfaces is wasted effort.

So it would be nice if we could reuse external signature declarations for
commonly needed concepts (again, like the concept of something string-like).
These shares signatures could be put in signature-only packages. Other
packages, instead of defining their own signatures, would import them (perhaps
renamed) from the signature packages.

There's a catch, though. A source of friction.

Authors of signature packages will want them to cover a lot of ground, to
include many useful functions for the given abstraction. All the functions that
make sense for string-like things, for example. But *consumers* of signature
packages are likely to need only a *subset* of all the functions in the
signature. And if they depend in the signature wholesale, that will constrain
the possible implementations, because the implementations will need to provide
implementation for functions *that they don't need*.

That's where signature thining comes in. When we depend on a signature-only
package (and this *only* works for signature-only packages) we have the option
to select which declarations do we want to keep in our local version of the
signature, and discard others. We get "pruned" signature that will be easier to
satisfy later.

The mechanism to do it is simple: declare a signature with the same name, but
only mention in the export list the types and functions in which we are
interested.

Whew, this is getting mightly confusing, isn't it? Let's go through the example
code.


```
cabal new-build
```
Run the executable with:

```
cabal new-run 
```
