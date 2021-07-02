module Lesson11.Mystery where

import Data.Functor.Identity

type Mystery = Identity

wrappedInMystery :: a -> Mystery a
wrappedInMystery = pure

