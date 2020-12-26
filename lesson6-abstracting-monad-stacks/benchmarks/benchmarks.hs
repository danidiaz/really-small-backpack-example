{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumDecimals #-}

module Main where

-- http://hackage.haskell.org/package/criterion-1.5.9.0/docs/Criterion.html#t:Benchmarkable

import Control.Monad.Reader
import Control.Monad.State.Strict
import Criterion.Main
import LogicIndef qualified
import LogicMTL qualified
import LogicTrans qualified

benchmarkMTL :: (Int, Int) -> Benchmarkable
benchmarkMTL env =
  whnf
    ( \(limit, iteration) ->
        flip execState iteration
          $ flip runReaderT limit
          $ LogicMTL.countUp
    )
    env

benchmarkTrans :: (Int, Int) -> Benchmarkable
benchmarkTrans env =
  whnf
    ( \(limit, iteration) ->
        flip execState iteration
          $ flip runReaderT limit
          $ LogicTrans.countUp
    )
    env

benchmarkIndef :: (Int, Int) -> Benchmarkable
benchmarkIndef env =
  whnf
    ( \(limit, iteration) ->
        flip execState iteration
          $ flip runReaderT limit
          $ LogicIndef.countUp
    )
    env

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "countUp"
        [ env (pure (10e6, 0)) $ bench "mtl" . benchmarkMTL,
          env (pure (10e6, 0)) $ bench "trans" . benchmarkTrans,
          env (pure (10e6, 0)) $ bench "indef" . benchmarkIndef
        ]
    ]
