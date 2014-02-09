{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Numeric.NumType.TF78 where

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
data NumType = P Nat | N Nat | Z deriving Typeable

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

type family Negate (n::NumType) :: NumType
  where
    Negate Z = Z
    Negate (P 0) = Z -- Should never occur
    Negate (N 0) = Z -- Should never occur
    Negate (P n) = (N n)
    Negate (N n) = (P n)

type family Succ (n::NumType) :: NumType
  where
    Succ Z = P 1
    Succ (P n) = P (n T.+ 1)
    Succ (N 0) = P 1 -- Should never occur
    Succ (N 1) = Z
    Succ (N n) = N (n T.- 1) 

type family Pred (n::NumType) :: NumType
  where
    Pred Z = N 1
    Pred (N n) = N (n T.+ 1)
    Pred (P 0) = N 1 -- Should never occur
    Pred (P 1) = Z
    Pred (P n) = P (n T.- 1) 

type family (n::NumType) + (m::NumType) :: NumType
  where
    Z + i = i
    i + Z = i -- Redundant.
    P n + P m = P (n T.+ m)
    N n + N m = N (n T.+ m)
    N n + i = Succ (N n) + Pred i
    P n + i = Pred (P n) + Succ i

type family (n::NumType) - (m::NumType) :: NumType
  where
    i - j = i + Negate j

type family (n::NumType) * (m::NumType) :: NumType
  where
    Z * i = Z
    i * Z = Z
    P n * P m = P (n T.* m)
    N n * N m = P (n T.* m)
    P n * N m = N (n T.* m)
    N n * P m = N (n T.* m)

type family (n::NumType) / (m::NumType) :: NumType
  where
    Z / P n = Z
    Z / N n = Z
  --Div (P n) / (P m) = Succ (Div (P n - P m) (P m))
    P n / P m = P (n ./ m)
    N n / N m = P (n ./ m)
    P n / N m = N (n ./ m)
    N n / P m = N (n ./ m)

-- Division for Nats. Will behave badly for divide by zero.
-- Used only for to help define the @(/)@ type family.
type family (n::Nat) ./ (m::Nat) :: Nat
  where
    0 ./ n = 0                       -- Will return 0 for n = 0.
    n ./ m = 1 T.+ ((n T.- m) ./ m)  -- Will infinite loop for m = 0?


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
(+) :: NT i -> NT j -> NT (i + j); (+) = undefined
(-) :: NT i -> NT j -> NT (i - j); (-) = undefined
(*) :: NT i -> NT j -> NT (i * j); (*) = undefined
(/) :: NT i -> NT j -> NT (i / j); (/) = undefined
negate :: NT i -> NT (Negate i); negate = undefined


-- | Conversion to @Integer@.
class ToInteger nt where toInteger :: nt -> Integer

instance ToInteger NZ where toInteger = const 0

instance KnownNat n => ToInteger (NP n) where
  toInteger = natVal . (undefined :: NP n -> Proxy n)

instance KnownNat n => ToInteger (NN n) where
  toInteger = Prelude.negate . natVal . (undefined :: NN n -> Proxy n)
--toInteger = Prelude.negate . toInteger . negate -- More complicated constraint.


-- | Conversion to @Num@ instance.
toNum :: (ToInteger nt, Num a) => nt -> a
toNum = fromInteger . toInteger


-- Show instance.
instance ToInteger (NT i) => Show (NT i) where show = (++"#") . show . toInteger


main = do
    putStrLn "Hola"
    print $ pos3 + neg2
    print $ toInteger zero
    print $ toInteger neg1
    print $ toInteger pos1
    print $ pos3 * neg2
    print $ pos3 / neg1
    print $ pos3 * neg2 / neg3
