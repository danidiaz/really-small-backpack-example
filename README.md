# really-small-backpack-example

This is a small tutorial on the very basics of the Backpack module system.

It requires [cabal-install >= 3.2.0.0](https://www.haskell.org/cabal/) and ghc >= 8.10.2. You can install both using [ghcup](https://www.haskell.org/ghcup/).

It is a [multi-package project](https://cabal.readthedocs.io/en/3.4/nix-local-build.html#developing-multiple-packages). You can build all the packages from this folder using:

```
cabal build all
```

Any corrections welcome!

# Further resources for learning Backpack

## User guides

- A detailed description in the GHC user guide of [what can be put into a module
signature](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/separate_compilation.html#module-signatures).
This is Backpack seen from the language side.

- [The Cabal user guide](https://cabal.readthedocs.io/en/3.4/index.html),
in particular the sections about
[signatures](https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-library-signatures)
and
[mixins](https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-mixins).
This is Backpack seen from the package manager side.

## The theory behind Backpack

- Edward Z. Yang's [thesis](https://github.com/ezyang/thesis/releases) is quite
readable and gives a good account of the motivations for Backpack.

- Edward Z. Yang's [blog](http://blog.ezyang.com/category/haskell/backpack/).

- Scott Kilpatrick's
[thesis](https://www.reddit.com/r/haskell/comments/e7gopg/new_haskell_phd_thesis_on_backback_foundations/).
Not as directly applicable to the current implementation of Backpack as Edward
Z. Yang's thesis, but useful insights can be gleaned here.

## Other stuff

- The [Backpack](https://wiki.haskell.org/Backpack) and [Module
Signature](https://wiki.haskell.org/Module_signature) entries in the
Haskellwiki.

- I wrote a few Backpack tips & tricks
[here](https://medium.com/@danidiaz/backpacking-tips-3adb727bb8f7) and
[here](https://medium.com/@danidiaz/backpacking-tips-ii-47fa86e5bf2).

- [Example of an abstract package on Hackage which uses
signatures](http://hackage.haskell.org/package/unpacked-containers). Discussed
[here](https://www.reddit.com/r/haskell/comments/8a5w1n/new_package_unpackedcontainers/).

- [A signature for streaming libraries](https://github.com/danidiaz/streamy)

- [picnic: put containers into a
  backpack](https://kowainik.github.io/posts/2018-08-19-picnic-put-containers-into-a-backpack)
  [reddit](https://www.reddit.com/r/haskell/comments/98jegn/blog_post_picnic_put_containers_into_a_backpack/)
  Features an interesting example of abstracting over classes. (See also lesson
  5 of this tutorial.)

- [a blog post about common stanzas in .cabal
  files](https://vrom911.github.io/blog/common-stanzas). Not directly related
  to Backpack, but both `build-depends` and `mixins` fields can be put into
  common stanzas, which can then be imported by multiple libraries/executables
  in the package. This can help avoid duplication in the .cabal file. [An
  example](https://stackoverflow.com/a/59740286/1364288).

- [using Backpack to get around problems with overlapping
  instances](https://www.reddit.com/r/haskell/comments/f3b0ie/ann_acts_semigroup_actions_groups_and_torsors/fhk4wpw/)

- [Monad Transformers and Effects with Backpack](https://blog.ocharles.org.uk/posts/2020-12-23-monad-transformers-and-effects-with-backpack.html). [reddit](https://www.reddit.com/r/haskell/comments/kjer0o/monad_transformers_and_effects_with_backpack/).

