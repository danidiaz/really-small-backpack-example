{-# LANGUAGE FlexibleContexts #-}
module LogicIndef where

import Control.Monad.Reader
import Control.Monad.State.Strict
import LogicIndef.Monad (M) -- this is the signature

countUp :: M ()
countUp = do
  limit <- ask
  iteration <- get
  if iteration < limit
    then do
      modify' succ
      countUp
    else return ()
