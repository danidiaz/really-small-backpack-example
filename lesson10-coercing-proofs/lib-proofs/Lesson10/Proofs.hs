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

module Lesson10.Proofs where

import Data.Kind
import Data.Nat
import Data.Singletons
import Data.Type.Equality

mPlusZero :: forall (m :: Nat). Sing m -> m `NatPlus` Z :~: m
mPlusZero SZ = Refl
mPlusZero (SS n) = case mPlusZero n of Refl -> Refl

mPlusSucc :: forall (n :: Nat) (m :: Nat). Sing m -> (m `NatPlus` S n) :~: S (m `NatPlus` n)
mPlusSucc SZ = Refl
mPlusSucc (SS m') = case mPlusSucc @n m' of Refl -> Refl
