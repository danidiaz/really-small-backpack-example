{-# LANGUAGE ConstraintKinds #-}
module MappyOrdered (Key,M.Map,lookup,M.fromList) where

import qualified Data.Map.Strict as M

type Key = Ord

lookup :: (Eq k,Key k) => k -> M.Map k a -> Maybe a
lookup = M.lookup
