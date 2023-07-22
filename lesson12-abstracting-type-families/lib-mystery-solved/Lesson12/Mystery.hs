{-# LANGUAGE KindSignatures #-} 
{-# LANGUAGE StandaloneKindSignatures #-} 
{-# LANGUAGE TypeFamilies #-} 
module Lesson12.Mystery where

import Data.Functor.Identity
import Data.Text

import Data.Kind

type Mystery :: Type -> Type
type family Mystery x where
    Mystery Text = Text
    Mystery Int = ()