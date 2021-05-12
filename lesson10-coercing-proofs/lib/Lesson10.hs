{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Lesson10 where

import Lesson10.Proofs 

import Data.Kind
import Data.Nat
import Data.Singletons
import Data.Type.Equality

import Prelude hiding (reverse)

type Vec :: Nat -> Type -> Type
data Vec n a where
  Nil :: Vec Z a
  (:>) :: a -> Vec n a -> Vec (S n) a

infixr 5 :>

deriving stock instance Show a => Show (Vec n a)
deriving stock instance Eq a => Eq (Vec n a)

reverse :: Vec n a -> Vec n a
reverse ys = go SZ Nil ys
  where
    go :: forall m p a. Sing m -> Vec m a -> Vec p a -> Vec (m `NatPlus` p) a
    go m acc Nil = case mPlusZero m of Refl -> acc
    go m acc (x :> (xs :: Vec p_pred a)) = case mPlusSucc @p_pred m of Refl -> go (SS m) (x :> acc) xs

