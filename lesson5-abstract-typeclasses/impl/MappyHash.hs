{-# LANGUAGE ConstraintKinds #-}
module MappyHash (Key,M.map,M.lookup,M.fromList) where

import qualified Data.HashMap.Strict as M
import Data.Hashable

type Key k = (Eq k,Hashable k)
