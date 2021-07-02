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

module Main where

import Prelude hiding (reverse)
import Lesson10
import Data.Nat
import Data.Singletons
import Test.Tasty
import Test.Tasty.HUnit

vec :: Vec (S (S (S Z))) Char
vec = 'a' :> 'b' :> 'c' :> Nil

tests =
  testGroup
    "All"
    [ testCase "reverse" $
        assertEqual "" vec $
            reverse $ reverse $ vec
    ]

main :: IO ()
main = defaultMain tests
