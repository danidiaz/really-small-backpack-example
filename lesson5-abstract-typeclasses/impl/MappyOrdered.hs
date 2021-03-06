{-# LANGUAGE ConstraintKinds #-}
module MappyOrdered (Key,M.Map,lookup,fromList) where

import Prelude (Maybe,Eq,Ord)
import qualified Data.Map.Strict as M

type Key = Ord

lookup :: (Eq k,Key k) => k -> M.Map k a -> Maybe a
lookup = M.lookup

fromList :: (Eq k,Key k) => [(k, v)] -> M.Map k v
fromList = M.fromList
