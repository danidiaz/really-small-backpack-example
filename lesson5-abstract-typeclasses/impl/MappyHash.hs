{-# LANGUAGE ConstraintKinds #-}
module MappyHash (Key,Map,lookup,M.fromList) where

import Prelude ()
import qualified Data.HashMap.Strict as M
import Data.Hashable

type Key = Hashable

type Map = M.HashMap

lookup :: (Eq k,Key k) => k -> Map k a -> Maybe a
lookup = M.lookup
