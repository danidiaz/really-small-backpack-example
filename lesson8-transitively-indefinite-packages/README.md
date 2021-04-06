# lesson 8 - transitively indefinite packages

(This lesson was previously in a [separate
repository](https://twitter.com/DiazCarrete/status/1379126087637143556).)

We call a package "indefinite" if it has an unfilled module signature. 

A package can be indefinite because: 

- It directly contains a module signature
- It has a dependency on another indefinite package, and doesn't fill the
  other package's signatures. In that case, the unfilled signatures are
  "carried over".

This lesson contains examples of both types. We have 

- A library `core` which directly contains a module signature.
- A library `intermediate1` which depends on `core`.
- A library `intermediate2` which depends on `intermediate1`.
- A library `lib-impl` which is a compatible implementation for the
  module signature, but doesn't know (and must not know!) about any of the
  previous packages.
- An executable that depends on `intermediate2` and `lib-impl`.

`intermediate1` and `intermediate2` are indefinite packages, even if they don't
explicitly have a
[`signatures:`](https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-signatures)
field in their cabal files. Instead, the signature is inherited from `core`.

Compile and run the lesson with

    cabal run lesson8-transitively-indefinite-packages

## Renaming inherited signatures

A "transitively indefinite" package might wish to rename an inherited
signature. Why would this be useful? 

- One possible reason is bringing the signature under the "domain" of the
  package. That way clients that depend on the package encounter a signature
  with the same root module name as the package.
- Related to the above, having its own copy of the signature will mean that
  when the signature is filled, it will be filled for the package only.
  
As everything renaming-related in Backpack, this is done with a `mixins:`
clause.

For example, we could add the following to `library intermediate2` in the Cabal
file:

    mixins:
     core requires (Core.SomeSig as Intermediate1.SomeSig)

(Note the use of `requires`. As seen in earlier lessons, `requires` is, well,
required when we want to rename module signatures.)

Once we have renamed the signature, what other things should we change for the
lesson to compile? Must `intermediate2` change? Must the executable change?
This is left as as exercise.

## See also

- [The Backpack explanation in the Cabal User
  Guide](https://cabal.readthedocs.io/en/latest/cabal-package.html#backpack) 

>  A Backpack package is defined by use of the library:signatures field, or by
>  (transitive) dependency on a package that defines some requirements. 

