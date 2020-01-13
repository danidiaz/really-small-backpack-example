{-# LANGUAGE ConstraintKinds #-}
module MappyHash (Key,Map,M.lookup,M.fromList) where

import qualified Data.HashMap.Strict as M
import Data.Hashable

type Key k = (Eq k,Hashable k)

type Map k v = Hashmap k v
