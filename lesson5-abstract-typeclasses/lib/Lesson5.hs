-- | This module creates a map with `Int` keys,
--   but it ignores the exact implementation of the map.
--   That is abstracted by the `Mappy` signature.
module Lesson5 (doStuff) where

import Mappy

doStuff :: IO ()
doStuff = do
    print $ Mappy.lookup (1::Int) (Mappy.fromList [(1,True),(2,False)])
