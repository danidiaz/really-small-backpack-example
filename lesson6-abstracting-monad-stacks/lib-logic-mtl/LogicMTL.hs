{-# LANGUAGE FlexibleContexts #-}
module LogicMTL where

import Control.Monad.Reader
import Control.Monad.State.Strict

countUp :: (MonadReader Int m, MonadState Int m) => m ()
countUp = do
  limit <- ask
  iteration <- get
  if iteration < limit
    then do
      modify' succ
      countUp 
    else return ()
