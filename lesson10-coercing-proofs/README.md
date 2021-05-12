# lesson 10 - coercing proofs

(This lesson requires GHC 9.0 or newer.)

In [this video about implementing reverse for dependently-typed vectors](https://www.youtube.com/watch?v=jPZciAJ0oaw) it is mentioned that type equality proofs involving [singletons](http://hackage.haskell.org/package/singletons) are actually executed at runtime, and in fact can be costly to compute.

The solution proposed in the video is to use a combination of [`RULES`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/rewrite_rules.html?highlight=rules) and [`NOINLINE`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/pragmas.html?highlight=noinline#pragma-NOINLINE) to "coerce" the proofs to `Refl` instead of running them. 

This lesson proposes an alternative solution: put the type signatures of the proofs in a module signature, and then provide two implementations: one with actual proofs (used during tests), and another with "coerced proofs" (used in the executable).

The module `Lesson10` which defined the `Vec` type depends on the module signature, and therefore is indefinite. The main executable and the tests match it with the implementations. (There are no `mixins:` sections in the `.cabal` file becase the names already match.)

One potential disadvantage of this solution is that we might actually *forget* to compile the version with the non-coerced proofs!

Run the executable with:

> cabal run lesson10-coercing-proofs:exe:lesson10

Run the tests with:

> cabal test lesson10-coercing-proofs:test:tests

## See also

[This](https://www.reddit.com/r/haskell/comments/na1n08/video_tutorial_using_proofs_to_make_functions/gxr8396/)  comment on Reddit.
