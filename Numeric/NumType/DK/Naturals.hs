{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.NumType.DK.Naturals where

import Prelude hiding (pred)

-- Use the same fixity for operators as the Prelude.
infixr 8  ^
infixl 7  *
infixl 6  +

data TypeNat = Z | S TypeNat  -- Natural numbers starting at 0.

-- | Nat addition.
type family (n::TypeNat) + (n'::TypeNat) :: TypeNat where
  -- Z + n = n  -- Redundant.
  n + 'Z = n
  n + 'S n' = 'S n + n'

-- | Nat subtraction.
type family (n::TypeNat) - (n'::TypeNat) :: TypeNat where
  n - 'Z = n
  'S n - 'S n' = n - n'

-- | Nat multiplication.
type family (n::TypeNat) * (n'::TypeNat) :: TypeNat
  where
    --Z * n = Z  -- Redundant
    n * 'Z = 'Z
    n * ('S n') = n + n * n'  -- i * Pos n

-- | Nat exponentiation.
type family (n::TypeNat) ^ (n'::TypeNat) :: TypeNat
  where
    --Zero ^ Pos n = Zero  -- Redundant.
    n ^ 'Z = 'S 'Z
    n ^ 'S n' = n * n ^ n'


class KnownTypeNat (n::TypeNat) where natVal :: proxy n -> Integer

instance KnownTypeNat 'Z where natVal _ = 0
instance KnownTypeNat n => KnownTypeNat ('S n) where
  natVal = (1 +) . natVal . pred
    where
      pred :: proxy ('S n) -> proxy n
      pred = undefined
