{-# LANGUAGE KindSignatures #-} 
{-# LANGUAGE StandaloneKindSignatures #-} 
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE UndecidableInstances #-} 
module Lesson12.User (
  User (..),
  NormalMode,   
 ) where 

import Lesson12.Mystery
import Data.Text
import Data.Functor.Identity
import Data.Kind

data NormalMode = NormalMode

type Dispatch :: Type -> Type -> Type
type family Dispatch f x where
  Dispatch NormalMode x = x
  Dispatch mode x = Mystery mode x

data User f = User {
    name :: Dispatch f Text,
    age :: Dispatch f Int
 }