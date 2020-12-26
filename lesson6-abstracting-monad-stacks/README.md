# lesson 6 - abstracting monad stacks

*UNDER CONSTRUCTION*

To inspect Core:

    $ cabal build --ghc-options="-ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes" > core.txt


To inspect Core without suppressing module prefixes (useful for inspecting the Core of `Main.hs`):

    $ cabal build --ghc-options="-ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques > core.txt


