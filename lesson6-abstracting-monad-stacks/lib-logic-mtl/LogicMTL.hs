{-# LANGUAGE FlexibleContexts #-}

module LogicMTL where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

countUp :: (MonadReader Int m, MonadWriter (Sum Int) m, MonadState Int m) => m ()
countUp = do
  limit <- ask
  iteration <- get
  if iteration < limit
    then do
      tell (Sum 1)
      modify' succ
      countUp
    else return ()
