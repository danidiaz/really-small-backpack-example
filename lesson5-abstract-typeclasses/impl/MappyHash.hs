{-# LANGUAGE ConstraintKinds #-}
module MappyHash (Key,Map,lookup,fromList) where

import Prelude (Eq,Maybe)
import qualified Data.HashMap.Strict as M
import Data.Hashable

type Key = Hashable

type Map = M.HashMap

lookup :: (Eq k,Key k) => k -> Map k a -> Maybe a
lookup = M.lookup

fromList :: (Eq k,Key k) => [(k, v)] -> Map k v
fromList = M.fromList
