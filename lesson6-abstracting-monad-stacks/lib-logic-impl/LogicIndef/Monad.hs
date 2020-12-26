{-# LANGUAGE FlexibleContexts #-}
module LogicIndef.Monad (M) where

import Control.Monad.Reader
import Control.Monad.State.Strict

type M = ReaderT Int (State Int)

