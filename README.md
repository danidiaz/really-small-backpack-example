# really-small-backpack-example

This is a small tutorial on the very basics of the Backpack module system.
(Well... at least it started that way. It has grown a bit over time.)

It requires [`cabal-install >= 3.2.0.0`](https://www.haskell.org/cabal/) and `GHC >= 9.0.1`. You can install both using [ghcup](https://www.haskell.org/ghcup/).

It is a [multi-package
project](https://cabal.readthedocs.io/en/3.4/nix-local-build.html#developing-multiple-packages).
You can build all the packages from this folder using:

```
cabal build all
```

Any corrections welcome!

# lessons

- [00 - convenience libraries](./lesson0-convenience-libraries)
- [01 - renaming modules](./lesson1-renaming-modules)
- [02 - signatures](./lesson2-signatures)
- [03 - signature merging](./lesson3-signature-merging)
- [04 - signature thinning](./lesson4-signature-thinning)
- [05 - abstract typeclasses](./lesson5-abstract-typeclasses)
- [06 - abstracting monad stacks](./lesson6-abstracting-monad-stacks)
- [07 - module identity](./lesson7-module-identity)
- [08 - transitively indefinite packages](./lesson8-transitively-indefinite-packages)
- [09 - template haskell](./lesson9-template-haskell)
- [10 - coercing proofs](./lesson10-coercing-proofs)
- [11 - controlling encapsulation](./lesson11-controlling-encapsulation)

# Further resources for learning Backpack

## User guides

- A detailed description in the GHC User Guide of [what can be put into a module
signature](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/separate_compilation.html#module-signatures).
This is Backpack seen from the language side.

- [The Backpack section of the Cabal User
  Guide](https://cabal.readthedocs.io/en/latest/cabal-package.html#backpack)
  and also the sections about [module
  signatures](https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-signatures)
  and
  [mixins](https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-mixins).
  This is Backpack seen from the package manager side.

## The theory behind Backpack

- Edward Z. Yang's [thesis](https://github.com/ezyang/thesis/releases) is quite
readable and gives a good account of the motivations for Backpack.

- Edward Z. Yang's [blog](http://blog.ezyang.com/category/haskell/backpack/).

- Scott Kilpatrick's
[thesis](https://www.reddit.com/r/haskell/comments/e7gopg/new_haskell_phd_thesis_on_backback_foundations/).
Not as directly applicable to the current implementation of Backpack as Edward
Z. Yang's thesis, but useful insights can be gleaned here.

## The implementation

- [Backpack design ticket #3038](https://github.com/haskell/cabal/issues/3038).

- [relationship between ComponentId, UnitId, MungedPackageId, PackageId etc.](https://github.com/haskell/cabal/issues/5809).

- [about units](https://github.com/ghc/ghc/blob/ce1b8f4208530fe6449506ba22e3a05048f81564/compiler/GHC/Unit.hs#L25).

## Other Backpack-related stuff

- The [Backpack](https://wiki.haskell.org/Backpack) and [Module
Signature](https://wiki.haskell.org/Module_signature) entries in the
Haskellwiki.

- [GHC proposal](https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst). Interesting bits about the motivation and limits of Backpack.

- I wrote a few Backpack tips & tricks
[here](httpstter.com/geoffreylitt/status/1379579340925632512://medium.com/@danidiaz/backpacking-tips-3adb727bb8f7) and
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

- [version 0.3.0 of cryptographic library "raaz" uses Bapckpack internally](http://hackage.haskell.org/package/raaz-0.3.0). [ergonomic issues](https://www.reddit.com/r/haskell/comments/nkvdwp/ergonomic_issues_with_using_backpack/).

- [moo-nad](http://hackage.haskell.org/package/moo-nad) A tiny library that combines module signatures and the "`ReaderT` pattern".

- [a few extant bugs, and a possible way forward](https://discourse.haskell.org/t/hf-tech-proposal-1-utf-8-encoded-text/2499/22)


## Stuff about module systems in general

- [Understanding and Evolving the ML Module System](https://people.mpi-sws.org/~dreyer/thesis/main.pdf) by Mark Dreyer.

- [Modular Type Classes
  (2007)](http://people.mpi-sws.org/~dreyer/papers/mtc/main-long.pdf)
  reconstruction of Haskell typeclasses in terms of ML modules "classes are
  signatures, instances are modules" 

- [ML Modules and Haskell Type Classes: A Constructive Comparison" (2008)](http://priv.stefanwehr.de/publications/Wehr_ML_modules_and_Haskell_type_classes_SHORT.pdf])

- [Logical relations as
  types](https://twitter.com/jonmsterling/status/1386647300244639747) by
  Sterling and Harper. [video](https://www.youtube.com/watch?v=AEthjg2k718).
  This went way over my head, but it seems to propose a method to verify that a
  module implementation has the same external behaviour as some (presumably
  simpler) reference implementation.

- [Modules as dependent records tweet](https://twitter.com/jonmsterling/status/1390346042823086084). 

- [Do-it-Yourself Module Systems](https://twitter.com/Iceland_jack/status/1390863512684093448) PhD thesis.

- [Abstract Data Types without the Types](https://homepages.inf.ed.ac.uk/wadler/papers/turner-festschrift/turner-festschrift.pdf).

