{-# LANGUAGE ConstraintKinds #-}
module MappyOrdered (Key,M.map,M.lookup,M.fromList) where

import qualified Data.Map.Strict as M

type Key k = Ord k

