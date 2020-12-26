{-# LANGUAGE FlexibleContexts #-}
module LogicTrans where

import Control.Monad.Reader
import Control.Monad.State.Strict

countUp :: (ReaderT Int (State Int)) ()
countUp = do
  limit <- ask
  iteration <- get
  if iteration < limit
    then do
      modify' succ
      countUp
    else return ()
