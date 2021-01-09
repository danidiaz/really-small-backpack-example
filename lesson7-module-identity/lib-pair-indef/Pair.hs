module Pair (
    module Pair.Element, 
    Pair, 
    buildPair, 
    pairFst, 
    pairSnd) where

import Pair.Element (Element)

data Pair = Pair Element Element

buildPair :: Element -> Element -> Pair
buildPair = Pair

pairFst :: Pair -> Element
pairFst (Pair x _) = x

pairSnd :: Pair -> Element
pairSnd (Pair _ x)  = x
