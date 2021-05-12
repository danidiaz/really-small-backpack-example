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
import Unsafe.Coerce

mPlusZero :: forall (m :: Nat). Sing m -> m `NatPlus` Z :~: m
mPlusZero _ = unsafeCoerce Refl

mPlusSucc :: forall (n :: Nat) (m :: Nat). Sing m -> (m `NatPlus` S n) :~: S (m `NatPlus` n)
mPlusSucc _ = unsafeCoerce Refl
