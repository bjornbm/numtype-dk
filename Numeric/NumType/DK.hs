{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Numeric.NumType.DK where

import Data.Proxy
import Data.Typeable
import GHC.TypeLits hiding ((+)(), (*)(), (-)())
import qualified GHC.TypeLits as T
import Prelude hiding ((+), (-), (*), (/), negate, toInteger)
import qualified Prelude


-- Use the same fixity for operators as the Prelude.
infixl 7  *, /
infixl 6  +, -

-- This definition allows both @P 0@ and @N 0@ despite the fact
-- that only Z should be used. Behave!
--
-- I don't ses how I can constrain the @Nat@s at the kind level or otherwise
-- avoid this unfortunate situation without significant tradeoffs in
-- readability. One option would be to have e.g. @P n@ mean @n + 1@, but
-- that would make reading types much less intuitive.
data NumType = P Nat  -- ^ Positive numbers.
             | N Nat  -- ^ Negative numbers.
             | Z      -- ^ Zero.
             deriving Typeable


-- | NumType negation.
type family Negate (n::NumType) :: NumType
  where
    Negate Z = Z
    Negate (P n) = (N n)
    Negate (N n) = (P n)

-- | NumType addition.
type family (n::NumType) + (n'::NumType) :: NumType
  where
    Z + i = i
    i + Z = i
    P n + P n' = P (n T.+ n')
    N n + N n' = N (n T.+ n')
    P 1 + N 1 = Z
    P 1 + N n = N (n T.- 1)
    P n + N 1 = P (n T.- 1)
    P n + N n' = P (n T.- 1) + N (n' T.- 1)
    N n + P n' = P n' + N n  -- Commutativity.

-- | NumType subtraction.
type family (n::NumType) - (n'::NumType) :: NumType
  where
    i - i' = i + Negate i'

-- | NumType multiplication.
type family (n::NumType) * (n'::NumType) :: NumType
  where
    Z * i = Z
    i * Z = Z
    P n * P n' = P (n T.* n')
    N n * N n' = P (n T.* n')
    P n * N n' = N (n T.* n')
    N n * P n' = N (n T.* n')

-- | NumType division.
type family (i::NumType) / (i'::NumType) :: NumType
  where
    Z / P n = Z
    Z / N n = Z
    P n / N n' = Negate (P n / P n')
    N n / P n' = Negate (P n / P n')
    N n / N n' = P n / P n'
    i / i' = (i - i') / i' + Pos1  -- P n / P n'


-- Some type synonyms for convenience (and consistency with FD version).
type Zero = Z
type Pos1 = P 1
type Pos2 = P 2
type Pos3 = P 3
type Pos4 = P 4
type Pos5 = P 5
type Neg1 = N 1
type Neg2 = N 2
type Neg3 = N 3
type Neg4 = N 4
type Neg5 = N 5


-- Enough work at the kind/type level. Bringing it down to type/value level ...


-- | Proxy for 'NumType's.
data NT (n::NumType) deriving Typeable
--data NT :: NumType -> *

-- Shorthands for convenience.
type NZ   = NT  Z
type NP n = NT (P n)
type NN n = NT (N n)


-- Value level names.
zero :: NZ  ; zero = undefined
pos1 :: NP 1; pos1 = undefined
pos2 :: NP 2; pos2 = undefined
pos3 :: NP 3; pos3 = undefined
pos4 :: NP 4; pos4 = undefined
pos5 :: NP 5; pos5 = undefined
neg1 :: NN 1; neg1 = undefined
neg2 :: NN 2; neg2 = undefined
neg3 :: NN 3; neg3 = undefined
neg4 :: NN 4; neg4 = undefined
neg5 :: NN 5; neg5 = undefined

-- Value level operators.
(+) :: NT i -> NT i' -> NT (i + i'); (+) = undefined
(-) :: NT i -> NT i' -> NT (i - i'); (-) = undefined
(*) :: NT i -> NT i' -> NT (i * i'); (*) = undefined
(/) :: NT i -> NT i' -> NT (i / i'); (/) = undefined
negate :: NT i -> NT (Negate i); negate = undefined


-- | Conversion to @Integer@.
class ToInteger nt where
  toInteger :: nt -> Integer
instance ToInteger NZ where
  toInteger = const 0
instance KnownNat n => ToInteger (NP n) where
  toInteger = natVal . (undefined :: NP n -> Proxy n)
instance KnownNat n => ToInteger (NN n) where
  toInteger = Prelude.negate . natVal . (undefined :: NN n -> Proxy n)
--toInteger = Prelude.negate . toInteger . negate -- Complicates the constraint.


-- | Conversion to @Num@ instance.
toNum :: (ToInteger nt, Num a) => nt -> a
toNum = fromInteger . toInteger


-- Show instance.
instance ToInteger (NT i) => Show (NT i) where show = (++"#") . show . toInteger


{-
-- | Conversion from NumType to Nat.
type family ToNat (n::NumType) :: Nat
  where
    ToNat Z = 0
    ToNat (P n) = n

-- | Conversion from Nat to NumType.
type family FromNat (n::Nat) :: NumType
  where
    FromNat 0 = Z
    FromNat n = P n
-}

{-
-- Here is some modofications/addtional code that can be added to provide some
-- clues to type errors involving incorrect division.

{-# LANGUAGE FlexibleContexts #-}

type family Abs (n::NumType) :: NumType
  where
    Abs Z = Z
    Abs (P n) = P n
    Abs (N n) = P n

class Divide (i::NumType) (i'::NumType)
instance Divide Z (P n)
instance Divide Z (N n)
instance Divide (P n - P n') (P n') => Divide (P n) (P n')

(/) :: Divide (Abs i) (Abs i') => NT i -> NT i' -> NT (i / i'); (/) = undefined
-}
