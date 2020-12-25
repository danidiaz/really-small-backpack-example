{-# LANGUAGE FlexibleContexts #-}
module LogicIndef where

import LogicIndef.Monad

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

countUp :: M ()
countUp = do
    limit <- ask
    iteration <- get
    if iteration < limit
        then do
            tell (Sum 1)
            modify' succ
            countUp
        else return ()


