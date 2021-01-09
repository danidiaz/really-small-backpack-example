{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import Pair1 qualified
import Pair2 qualified

main :: IO ()
main = print $ Pair1.pairFst $ Pair2.buildPair 1 2


