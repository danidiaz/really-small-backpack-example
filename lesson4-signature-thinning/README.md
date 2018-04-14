# lesson 4 - signature thinning

Signature thinning (not to confuse with signature merging) is an experimental
(?) feature inteded to facilitate the reuse of signatures.

Let's start with one idea: there can exist packages that only contain
signatures, and no implementation modules. Why are they useful?

So far in this course, every library component with "holes" declared a
signature tailored to their particular needs. In time, these ad-hoc signatures
will multiply throughout the ecosystem. Many of them will cover the same
concepts, say, the abstract interface of a string-like type. And yet, they are
likely to be incompatible, because each author will have used different names
for the types and functions, or made slightly different decisions on the types.
Furthermore, re-inventing already existing interfaces is wasted effort.

So it would be nice if we could reuse external signature declarations for
commonly needed concepts (again, like the concept of something string-like).
These shared signatures could be put in signature-only packages. Other
packages, instead of defining their own signatures, would import them (perhaps
renamed) from the signature packages.

There's a catch, though. A source of friction.

Authors of signature packages will want them to cover a lot of ground, to
include many useful functions for the given abstraction. All the functions that
make sense for string-like things, for example. But *consumers* of signature
packages are likely to need only a *subset* of all the functions in the
signature. And if they depend on the signature wholesale, that will constrain
the possible implementation modules, because they will have to provide
implementations for functions *that aren't needed*.

That's where signature thinning comes in. When we depend on a signature-only
package (and this *only* works for signature-only packages) we have the option
to select which declarations do we want to keep in our local version of the
signature, and discard others. We get a "pruned" signature that will be easier
to satisfy later.

The mechanism to do it is simple: declare a signature with the same name, but
only mention in the export list the types and functions in which we are
interested.

Whew, this is getting mighty confusing, isn't it? Let's go through the example
code.

We have a signature-only library [justthesig](package.cabal#L54). It defines
two values `fooRequiresThis` and `barRequiresThis`.

Then we have to "libraries with holes" [foo](package.cabal#L26) and
[bar](package.cabal#L40). Each of them wants to reutilize `justthesig` in their
own signatures. They do two things:

- They [rename](package.cabal#L37) the `Siggy` signature into two signatures
  `Foo.Siggy` and `Bar.Siggy` that are "under their control" in their
  respective namespaces, and then [publish](package.cabal#L30) those signatures
  in their turn.

  Notice that signatures are renamed using the `requires` clause of the
  `mixins` section.

- They supply their own [Foo/Siggy.hsig](lib-foo/Foo/Siggy.hsig) and
  [Bar/Siggy.hsig](lib-bar/Bar/Siggy.hsig) signature files, that carefully
  export only those functions in which `foo` and `bar` are actually
  interested. This is where signature thinning takes place. (Why are those
  functions in scope? Because of signature merging.)

  In this example it seems that we aren't gaining a lot, but consider that the
  signatures could be bigger and more complicated. We didn't have to re-invent
  them!

Notice how the [two implementation modules](package.cabal#L16) used with `foo`
and `bar` only have to implement a part of the whole original **Siggy**
signature.

Compile in this folder with the command:

```
cabal new-build
```
Run the executable with:

```
cabal new-run 
```
