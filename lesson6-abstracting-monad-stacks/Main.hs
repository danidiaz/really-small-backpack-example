{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import LogicMTL qualified
import LogicTrans qualified
import LogicIndef qualified

import Control.Monad.Reader
import Control.Monad.State.Strict

main :: IO ()
main = do
    print $ flip execState 0 $ flip runReaderT 10 $ LogicMTL.countUp
    print $ flip execState 0 $ flip runReaderT 10 $ LogicTrans.countUp
    print $ flip execState 0 $ flip runReaderT 10 $ LogicIndef.countUp
