{-# LANGUAGE KindSignatures #-} 
{-# LANGUAGE StandaloneKindSignatures #-} 
{-# LANGUAGE TypeFamilies #-} 
module Lesson12.Mystery where

import Data.Functor.Identity
import Data.Text

import Data.Kind

data MysteryMode = MysteryMode

type Mystery :: Type -> Type -> Type
type family Mystery mode x where
    Mystery MysteryMode Text = Text
    Mystery MysteryMode Int = ()
