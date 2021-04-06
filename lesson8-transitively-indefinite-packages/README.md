# lesson 8 - transitively indefinite packages

(This lesson was previously in a [separate
repository](https://twitter.com/DiazCarrete/status/1379126087637143556).)

We call a package "indefinite" if it has an unfilled module signature. 

A package can be indefinite because: 

    - It directly imports a module signature

    - It has a dependency on another indefinite package, and doesn't fill the
      other package's signatures. In that case, the unfilled signatures are
      "carried over".

This lesson contains examples of both types. We have 

    - A library `core` which directly contains a module signature.
    - A library `intermediate1` which depends on `core`.
    - A library `intermediate2` which depends on `intermediate1`.
    - A library `lib-impl` which provides a compatible implementation for the
      module signature, but doesn't know (and must not know!) about any of the
      previous packages.
    - An executable that depends on `intermediate2` and `lib-impl`.

`intermediate1` and `intermediate2` are indefinite packages, even if they don't
explicitly have a
[`signatures:`](https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-signatures)
field in their cabal files. Instead, the signature is inherited from `core`.

Compile and run the lesson with

    cabal run lesson8-transitively-indefinite-packages

