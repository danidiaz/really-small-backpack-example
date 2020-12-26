# lesson 6 - abstracting monad stacks

*UNDER CONSTRUCTION*

This lesson is motivated by the interesting post [Monad Transformers and Effects with Backpack](https://blog.ocharles.org.uk/posts/2020-12-23-monad-transformers-and-effects-with-backpack.html) by **ocharles** and the [subsequent Reddit discussion](https://www.reddit.com/r/haskell/comments/kjer0o/monad_transformers_and_effects_with_backpack/).

I want to propose a more coarse-grained example of how abstract monad stacks using Backpack. The idea is as follows: instead of having fine-grained [module signatures](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/separate_compilation.html#module-signatures) for individual transformers, write a single module signature describing the monad in which your program logic should run. That signature can require constraints like `MonadReader` or `MonadState` from the abstract monad.

Then, in the implementation module, assemble your whole monad stack using garden-variety transformers from the ["transformers"](http://hackage.haskell.org/package/transformers) package.

In this package, library **lib-logic-indef** is the program logic which depends on a module signature `LogicIndef.Monad`, and library **lib-logic-impl** provides the implementation of the package. 

The logic is imported and run by the **lesson6** executable. In order to run it:

    cabal run lesson6

Actually, for purposes of comparison, the executable runs two other "program logics" from these libraries:

- **lib-logic-mtl**: The monad stack is abstracted using MTL classes; concrete transformers are only specified in the **lesson6** executable. 

- **lib-logic-trans**: The monad stack is not abstracted at all; concrete transformers are used in the signature.

In all three cases, the provided function (`countUp`) is *exactly the same*, the only thing that varies is if/how the concrete monad stack is abstracted. 

## Comparing the generated Core

For all three cases, the code is compiled with `-O2`. Also, none of the "program logics" is marked as [`INLINE`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=inlinable#inline-pragma).

We can run these commands to generate Core:

    cabal clean
    cabal build --ghc-options="-ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques" > core.txt

The Core generated for **lib-logic-mtl** doesn't look very efficient. Typeclass dictionaries are passed around with abandon:

    Rec {
    -- RHS size: {terms: 63, types: 113, coercions: 0, joins: 0/3}
    LogicMTL.$wcountUp
      :: forall (m :: * -> *).
         (forall a b. m a -> (a -> m b) -> m b)
         -> (forall a b. m a -> m b -> m b)
         -> (forall a. a -> m a)
         -> m Int
         -> MonadState Int m => m ()
    LogicMTL.$wcountUp
      = \ (@ (m :: * -> *))
          (ww :: forall a b. m a -> (a -> m b) -> m b)
          (ww1 :: forall a b. m a -> m b -> m b)
          (ww2 :: forall a. a -> m a)
          (ww3 :: m Int)
          (w :: MonadState Int m) ->
          let {
            lvl :: m Int
            lvl = get w } in
          let {
            lvl1 :: m ()
            lvl1 = ww2 ghc-prim-0.6.1:GHC.Tuple.() } in
          let {
            lvl2 :: m ()
            lvl2
              = ww1
                  (case w of
                   { Control.Monad.State.Class.C:MonadState ww5 ww6 ww7 ww8 ->
                   case ww5 of { GHC.Base.C:Monad ww10 ww11 ww12 ww13 ->
                   ww11
                     ww6
                     (\ (s' :: Int) ->
                        case s' of { ghc-prim-0.6.1:GHC.Types.I# x1 ->
                        case x1 of wild1 {
                          __DEFAULT ->
                            ww7
                              (ghc-prim-0.6.1:GHC.Types.I#
                                 (ghc-prim-0.6.1:GHC.Prim.+# wild1 1#));
                          9223372036854775807# -> case GHC.Enum.$fEnumInt2 of wild2 { }
                        }
                        })
                   }
                   })
                  (LogicMTL.$wcountUp ww ww1 ww2 ww3 w) } in
          ww
            ww3
            (\ (limit :: Int) ->
               ww
                 lvl
                 (\ (iteration :: Int) ->
                    case iteration of { ghc-prim-0.6.1:GHC.Types.I# x ->
                    case limit of { ghc-prim-0.6.1:GHC.Types.I# y ->
                    case ghc-prim-0.6.1:GHC.Prim.<# x y of {
                      __DEFAULT -> lvl1;
                      1# -> lvl2
                    }
                    }
                    }))
    end Rec }

    -- RHS size: {terms: 15, types: 62, coercions: 0, joins: 0/0}
    countUp
      :: forall (m :: * -> *).
         (MonadReader Int m, MonadState Int m) =>
         m ()
    countUp
      = \ (@ (m :: * -> *))
          (w :: MonadReader Int m)
          (w1 :: MonadState Int m) ->
          case w of
          { Control.Monad.Reader.Class.C:MonadReader ww1 ww2 ww3 ww4 ->
          case ww1 of { GHC.Base.C:Monad ww6 ww7 ww8 ww9 ->
          LogicMTL.$wcountUp ww7 ww8 ww9 ww2 w1
          }
          }


The Core generated for **lib-logic-trans** is shorter and more optimized, which isn't surprising, given that the compiler knows about the concrete types. No typeclass dictionaries in sight:

    LogicTrans.$wcountUp
      :: ghc-prim-0.6.1:GHC.Prim.Int#
         -> ghc-prim-0.6.1:GHC.Prim.Int# -> (# (), Int #)
    LogicTrans.$wcountUp
      = \ (ww :: ghc-prim-0.6.1:GHC.Prim.Int#)
          (ww1 :: ghc-prim-0.6.1:GHC.Prim.Int#) ->
          case ghc-prim-0.6.1:GHC.Prim.<# ww1 ww of {
            __DEFAULT ->
              (# ghc-prim-0.6.1:GHC.Tuple.(), ghc-prim-0.6.1:GHC.Types.I# ww1 #);
            1# ->
              case ww1 of wild1 {
                __DEFAULT ->
                  LogicTrans.$wcountUp ww (ghc-prim-0.6.1:GHC.Prim.+# wild1 1#);
                9223372036854775807# -> case GHC.Enum.$fEnumInt2 of wild { }
              }
          }
    end Rec }

    -- RHS size: {terms: 16, types: 15, coercions: 5, joins: 0/0}
    LogicTrans.countUp1
      :: Int -> Int -> Data.Functor.Identity.Identity ((), Int)
    LogicTrans.countUp1
      = \ (w :: Int) (w1 :: Int) ->
          case w of { ghc-prim-0.6.1:GHC.Types.I# ww1 ->
          case w1 of { ghc-prim-0.6.1:GHC.Types.I# ww3 ->
          case LogicTrans.$wcountUp ww1 ww3 of { (# ww5, ww6 #) ->
          (ww5, ww6) `cast` <Co:5>
          }
          }
          }


The Core generated for **lib-logic-indef** instantiated with **lib-logic-impl** is very similar to that of **lib-logic-trans**:

    LogicIndef.$wcountUp
      :: ghc-prim-0.6.1:GHC.Prim.Int#
         -> ghc-prim-0.6.1:GHC.Prim.Int# -> (# (), Int #)
    LogicIndef.$wcountUp
      = \ (ww :: ghc-prim-0.6.1:GHC.Prim.Int#)
          (ww1 :: ghc-prim-0.6.1:GHC.Prim.Int#) ->
          case ghc-prim-0.6.1:GHC.Prim.<# ww1 ww of {
            __DEFAULT ->
              (# ghc-prim-0.6.1:GHC.Tuple.(), ghc-prim-0.6.1:GHC.Types.I# ww1 #);
            1# ->
              case ww1 of wild1 {
                __DEFAULT ->
                  LogicIndef.$wcountUp ww (ghc-prim-0.6.1:GHC.Prim.+# wild1 1#);
                9223372036854775807# -> case GHC.Enum.$fEnumInt2 of wild { }
              }
          }
    end Rec }

    -- RHS size: {terms: 16, types: 15, coercions: 5, joins: 0/0}
    LogicIndef.countUp1
      :: Int -> Int -> Data.Functor.Identity.Identity ((), Int)
    LogicIndef.countUp1
      = \ (w :: Int) (w1 :: Int) ->
          case w of { ghc-prim-0.6.1:GHC.Types.I# ww1 ->
          case w1 of { ghc-prim-0.6.1:GHC.Types.I# ww3 ->
          case LogicIndef.$wcountUp ww1 ww3 of { (# ww5, ww6 #) ->
          (ww5, ww6) `cast` <Co:5>
          }
          }
          }

## A benchmark

A benchmark is included which compares the three versions of the logic. It can be run with:

    cabal bench

As expected, the MTL version of the logic runs much slower. But remember: we aren't inlining any of the `countUp` functions!

