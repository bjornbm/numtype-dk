{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.NumType.DK.Nat where


-- Use the same fixity for operators as the Prelude.
infixr 8  ^
infixl 7  *
infixl 6  +

data Nat = Z | S Nat  -- Natural numbers starting at 0.
--type N1 = S Z

-- | Nat addition.
type family (n::Nat) + (n'::Nat) :: Nat where
  Z + n = n
  n + Z = n
  n + S n' = S n + n'

-- | Nat subtraction.
type family (n::Nat) - (n'::Nat) :: Nat where
  n - Z = n
  S n - S n' = n - n'

-- | Nat multiplication.
type family (n::Nat) * (n'::Nat) :: Nat
  where
    --Z * n = Z  -- Redundant
    n * Z = Z
    n * (S n') = n + n * n'  -- i * Pos n

-- | Nat exponentiation.
type family (n::Nat) ^ (n'::Nat) :: Nat
  where
    --Zero ^ Pos n = Zero  -- Redundant.
    n ^ Z = S Z
    n ^ S n' = n * n ^ n'


class KnownNat (n::Nat) where natVal :: proxy n -> Integer
instance KnownNat Z where natVal _ = 0
instance KnownNat n => KnownNat (S n) where
  natVal = (1 +) . natVal . pred
    where
      pred :: proxy (S n) -> proxy n
      pred = undefined
