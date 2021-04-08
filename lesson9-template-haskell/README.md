# lesson 9 - template haskell

Combining Template Haskell with indefinite packages can net you some unpleasant surprises.

In this lesson, we have the following libraries:

- `core` An indefinite library with a module signature. It also defines some TH code.
- `intermediate` Another indefinite library which inherits its signature from `core`. It also does some splicing.
- `intermediate-th` A concrete library, which defines TH code equivalent to the one in `core`.
- `lib-impl` A concrete implementation library.
- `lesson9* The main executable, which depends on both *intermediate* and *lib-impl*.

You can check file `Intermediate.hs` to see the splice. The splice code from comes from the concrete library *intermediate-th*:

```
-- this works, no problem
$(Intermediate.TH.makeIdFunc ''Core.A)
```

If we compile and run the lesson with

    cabal run lesson9-template-haskell

Everything should work correctly.  

## Where thing go wrong: splice code from indefinite libraries

Suppose that in `Intermediate.hs`, instead of using the splice code from `intermediate-th`, we used
the splice code from `core`, which happens to be exactly identical:

```
-- $(Intermediate.TH.makeIdFunc ''Core.A)
$(Core.TH.makeIdFunc ''Core.A)
```

Alas, we get an obscure error:

    cabal run lesson9-template-haskell
    ...
	ghc: ^^ Could not load 'lesson9zmtemplatezmhaskellzm1zi0zi0zi0zminplacezmcorezmAfo5JIVDJVcCScFF1FkCSzz_CoreziTH_makeIdFunc_closure', dependency unresolved. See top entry above.
	GHC.ByteCode.Linker.lookupCE
	During interactive linking, GHCi couldn't find the following symbol:
	  lesson9zmtemplatezmhaskellzm1zi0zi0zi0zminplacezmcorezmAfo5JIVDJVcCScFF1FkCSzz_CoreziTH_makeIdFunc_closure
	This may be due to you not asking GHCi to load extra object files,
	archives or DLLs needed by your current session.  Restart GHCi, specifying
	the missing library using the -L/path/to/object/dir and -lmissinglibname
	flags, or simply by naming the relevant files on the GHCi command line.
	Alternatively, this link failure might indicate a bug in GHCi.
	If you suspect the latter, please report this as a GHC bug:
	  https://www.haskell.org/ghc/reportabug

What is happening here? Why the splice code from *core* causes an error?

I'm not 100% sure, but I think the reason is that *splice code from as-yet indefinite packages can't work*.

Why? When building an indefinite library like `core`, it gets typechecked, but
not compiled down to object code. We can't do that, because the library still
has "holes" in it! But that means that other indefinite libraries like
`intermediate` can't run the splice code when they themselves get typechecked,
because the splice code doesn't exist yet.

Moving the `Intermediate.TH` module to `intermediate` won't work either,
because intermediate itself is indefinite.

In a way, this is a more annoying version of the TH [stage restriction](https://markkarpov.com/tutorial/th.html#limitations-of-th).

## What if...

We added a dependency to `core` in `lesson9` and tried to splice `Core.TH.makeIdFunc` in `Main.hs` itself? Should it work, or not? What do you think?

## See also

- [This Template Haskell tutorial from Mark Karpov](https://markkarpov.com/tutorial/th.html)

- [template-haskell on Hackage](http://hackage.haskell.org/package/template-haskell)

