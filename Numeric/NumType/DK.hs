{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.NumType.DK where

import Data.Proxy
import Prelude hiding ((+), (-), (*), (/), (^), pred, succ, negate, abs, signum)
import qualified Prelude

-- {-
-- Use custom @Typeable@ @Nat@s.
import Numeric.NumType.DK.Nat (Nat (S, Z))
import qualified Numeric.NumType.DK.Nat as N

type N1 = S Z  -- NumType.DK.Nats
-- -}

{-
-- Use @Nat@s from @GHC.TypeLits@ (not @Typeable@ as of GHC 7.8.1).
import GHC.TypeLits hiding ((+)(), (*)(), (-)(), (^)())
import qualified GHC.TypeLits as N

type Z  = 0  -- GHC.TypeLits
type N1 = 1  -- GHC.TypeLits
-- -}

-- Use the same fixity for operators as the Prelude.
infixr 8  ^
infixl 7  *, /
infixl 6  +, -


-- Natural numbers
-- ===============

type family NatPred (n::Nat) :: Nat where NatPred n = n N.- N1
type family NatSucc (n::Nat) :: Nat where NatSucc n = n N.+ N1


-- Integers
-- ========

data NumType = Pos1Plus  Nat  -- 1, 2, 3, ...
             | Zero      -- 0
             | Neg1Minus Nat  -- -1, -2, -3, ...

-- Type synonyms for convenience.
type Neg5 = Pred Neg4
type Neg4 = Pred Neg3
type Neg3 = Pred Neg2
type Neg2 = Pred Neg1
type Neg1 = Neg1Minus Z
type Pos1 = Pos1Plus  Z
type Pos2 = Succ Pos1
type Pos3 = Succ Pos2
type Pos4 = Succ Pos3
type Pos5 = Succ Pos4


-- Unary operations
-- ----------------

type family Pred (i::NumType) :: NumType where
  Pred Zero = Neg1
  Pred Pos1 = Zero
  Pred (Pos1Plus  n) = Pos1Plus  (NatPred n)
  Pred (Neg1Minus n) = Neg1Minus (NatSucc n)

type family Succ (i::NumType) :: NumType where
  Succ Zero = Pos1
  Succ Neg1 = Zero
  Succ (Neg1Minus n) = Neg1Minus (NatPred n)
  Succ (Pos1Plus  n) = Pos1Plus  (NatSucc n)

-- | NumType negation.
type family Negate (i::NumType) :: NumType where
  Negate Zero = Zero
  Negate (Pos1Plus  n) = Neg1Minus n
  Negate (Neg1Minus n) = Pos1Plus  n

-- | Absolute value.
type family Abs (i::NumType) :: NumType where
  Abs (Neg1Minus n) = Pos1Plus n
  Abs i = i  -- Abs (Pos n) or Abs Zero

-- | Signum.
type family Signum (i::NumType) :: NumType where
  Signum (Neg1Minus n) = Neg1
  Signum Zero          = Zero
  Signum i             = Pos1  -- Pos1Plus n


-- Binary operations
-- -----------------

-- | NumType addition.
type family (i::NumType) + (i'::NumType) :: NumType where
  Zero + i = i
  i + Zero = i
  i + Pos1Plus n = Succ i + Pred (Pos1Plus n)
  i + i'         = Pred i + Succ i'  -- i + Neg n

-- | NumType subtraction.
type family (i::NumType) - (i'::NumType) :: NumType where
  i - i' = i + Negate i'

-- | NumType multiplication.
type family (i::NumType) * (i'::NumType) :: NumType
  where
    Zero * i = Zero
    i * Zero = Zero
    i * Neg1Minus n = Negate (i * Pos1Plus n)
    i * i' = i + i * Pred i'  -- i * Pos n
    --i * Neg1 = Negate i
    --i * Neg n = Negate (i * Negate (Neg n))

-- | NumType division.
type family (i::NumType) / (i'::NumType) :: NumType
  where
    -- `Zero / n = Zero` would allow division by zero.
    Zero / Pos1Plus  n = Zero
    Zero / Neg1Minus n = Zero
    i / Neg1Minus n = Negate (i / Pos1Plus n)
    Neg1Minus n / i = Negate (Pos1Plus n / i)
    i / i' = (i - i') / i' + Pos1  -- Pos n / Pos n'

-- | NumType exponentiation.
type family (i::NumType) ^ (i'::NumType) :: NumType
  where
    i ^ Zero = Pos1
    --Zero ^ Pos n = Zero  -- Redundant.
    i ^ Pos1Plus n = i * i ^ Pred (Pos1Plus n)


-- Term level
-- ==========

-- Term level operators
-- --------------------

pred   :: Proxy i -> Proxy (Pred   i); pred   _ = Proxy
succ   :: Proxy i -> Proxy (Succ   i); succ   _ = Proxy
negate :: Proxy i -> Proxy (Negate i); negate _ = Proxy
abs    :: Proxy i -> Proxy (Abs    i); abs    _ = Proxy
signum :: Proxy i -> Proxy (Signum i); signum _ = Proxy

(+) :: Proxy i -> Proxy i' -> Proxy (i + i'); _ + _ = Proxy
(-) :: Proxy i -> Proxy i' -> Proxy (i - i'); _ - _ = Proxy
(*) :: Proxy i -> Proxy i' -> Proxy (i * i'); _ * _ = Proxy
(/) :: Proxy i -> Proxy i' -> Proxy (i / i'); _ / _ = Proxy
(^) :: Proxy i -> Proxy i' -> Proxy (i ^ i'); _ ^ _ = Proxy


-- Term level TypeNats for convenience
-- -----------------------------------

neg5 = Proxy :: Proxy Neg5
neg4 = Proxy :: Proxy Neg4
neg3 = Proxy :: Proxy Neg3
neg2 = Proxy :: Proxy Neg2
neg1 = Proxy :: Proxy Neg1
zero = Proxy :: Proxy Zero
pos1 = Proxy :: Proxy Pos1
pos2 = Proxy :: Proxy Pos2
pos3 = Proxy :: Proxy Pos3
pos4 = Proxy :: Proxy Pos4
pos5 = Proxy :: Proxy Pos5


-- Reification
-- -----------

-- | Conversion to a @Num@.
class KnownNumType (i::NumType) where toNum :: Num a => Proxy i -> a

instance KnownNumType Zero where toNum _ = 0

instance KnownNumType (Pred (Pos1Plus n)) => KnownNumType (Pos1Plus n)
  where toNum = (1 Prelude.+) . toNum . pred

instance KnownNumType (Pos1Plus n) => KnownNumType (Neg1Minus n)
  where toNum = Prelude.negate . toNum . negate
