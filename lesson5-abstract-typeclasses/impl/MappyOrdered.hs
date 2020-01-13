{-# LANGUAGE ConstraintKinds #-}
module MappyOrdered (Key,M.Map,M.lookup,M.fromList) where

import qualified Data.Map.Strict as M

type Key = Ord

