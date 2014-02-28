{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Numeric.NumType.DKTests where

import Numeric.NumType.DK
import Prelude hiding ((*), (/), (+), (-), (^), negate, abs, signum)
import qualified Prelude as P ((*), (/), (+), (-), (^), negate, abs, signum)
import Data.Proxy
import Test.HUnit


-- | Compares a type level unary function with a value level unary function
  -- by converting 'NumType' to a @Num@. This assumes that the 'toNum'
  -- function is solid.
unaryTest :: (KnownNumType i, KnownNumType i', Num a, Eq a, Show a)
          => (Proxy i -> Proxy i') -> (a -> a) -> Proxy i -> Test
unaryTest f f' x = TestCase $ assertEqual
    "Unary function Num equivalence"
    (f' (toNum x)) (toNum (f x))

-- | 'unaryTest' with @Num a@ fixed to @Integer@. This is needed by
  -- 'testIncrDecr'.
unaryTest' :: (KnownNumType i, KnownNumType i')
          => (Proxy i -> Proxy i') -> (Integer -> Integer) -> Proxy i -> Test
unaryTest' = unaryTest

-- | Compares a type level binary function with a value level binary function
  -- by converting 'NumType' to 'Num'. This assumes that the 'toNum'
  -- function is solid.
binaryTest :: (KnownNumType i, KnownNumType i', KnownNumType i'', Num a, Eq a, Show a)
           => (Proxy i -> Proxy i' -> Proxy i'')
           -> (a -> a -> a)
           -> Proxy i -> Proxy i' -> Test
binaryTest f f' x y = TestCase $ assertEqual
    "Binary function Num equivalence"
    (f' (toNum x) (toNum y)) (toNum (f x y))

binaryTest' :: (KnownNumType i, KnownNumType i', KnownNumType i'', Num a, Eq a, Show a)
           => (Proxy i -> Proxy i' -> Proxy i'')
           -> (Integer -> Integer -> Integer)
           -> Proxy i -> Proxy i' -> Test
binaryTest' = binaryTest


-- | Test that conversion to 'Num a' works as expected. This is sort of a
  -- prerequisite for the other tests.
testAsIntegral = TestLabel "Num equivalence tests" $ TestList
    [ TestCase $ -2 @=? toNum neg2
    , TestCase $ -1 @=? toNum neg1
    , TestCase $  0 @=? toNum zero
    , TestCase $  1 @=? toNum pos1
    , TestCase $  2 @=? toNum pos2
    ] -- By induction all other NumTypes should be good if these are.

-- | Test incrementing and decrementing. This test was more relevant for
  -- numtype-dk-0.1.
testIncrDecr = TestLabel "Increment and decrement tests" $ TestList
    [ t neg2
    , t neg1
    , t zero
    , t pos1
    , t pos1
    ] where
        t x = TestList [ unaryTest' (+ pos1) (P.+ 1) x
                       , unaryTest' (+ neg1) (P.- 1) x
                       , unaryTest' (\x -> x - neg1) (P.+ 1) x
                       , unaryTest' (\x -> x - pos1) (P.- 1) x
                       ]

-- | Test negation.
testNegate = TestLabel "Negation tests" $ TestList
    [ unaryTest negate P.negate neg2
    , unaryTest negate P.negate neg1
    , unaryTest negate P.negate zero
    , unaryTest negate P.negate pos1
    , unaryTest negate P.negate pos1
    ]

-- | Test absolute value.
testAbs = TestLabel "Absolute value tests" $ TestList
    [ unaryTest abs P.abs neg2
    , unaryTest abs P.abs neg1
    , unaryTest abs P.abs zero
    , unaryTest abs P.abs pos1
    , unaryTest abs P.abs pos1
    ]

-- | Test signum.
testSignum = TestLabel "Signum tests" $ TestList
    [ unaryTest signum P.signum neg2
    , unaryTest signum P.signum neg1
    , unaryTest signum P.signum zero
    , unaryTest signum P.signum pos1
    , unaryTest signum P.signum pos1
    ]

-- | Test addition.
testAddition = TestLabel "Addition tests" $ TestList
    [ binaryTest (+) (P.+) pos2 pos3
    , binaryTest (+) (P.+) neg2 pos3
    , binaryTest (+) (P.+) pos2 neg3
    , binaryTest (+) (P.+) neg2 neg3
    ]

-- | Test subtraction.
testSubtraction = TestLabel "Subtraction tests" $ TestList
    [ binaryTest (-) (P.-) pos2 pos5
    , binaryTest (-) (P.-) neg2 pos5
    , binaryTest (-) (P.-) pos2 neg5
    , binaryTest (-) (P.-) neg2 neg5
    ]

-- | Test multiplication.
testMultiplication = TestLabel "Multiplication tests" $ TestList
    [ binaryTest (*) (P.*) pos2 pos5
    , binaryTest (*) (P.*) neg2 pos5
    , binaryTest (*) (P.*) pos2 neg5
    , binaryTest (*) (P.*) neg2 neg5
    , binaryTest (*) (P.*) pos2 zero
    , binaryTest (*) (P.*) neg2 zero
    , binaryTest (*) (P.*) zero pos5
    , binaryTest (*) (P.*) zero neg5
    ]

-- | Test division.
testDivision = TestLabel "Division tests" $ TestList
    [ binaryTest (/) (P./) pos4 pos2
    , binaryTest (/) (P./) zero pos5
    , binaryTest (/) (P./) zero neg3
    , binaryTest (/) (P./) neg4 pos2
    , binaryTest (/) (P./) pos4 neg2
    , binaryTest (/) (P./) neg4 neg2
    , binaryTest (/) (P./) pos5 pos5
    ]

-- | Test exponentiation.
testExponentiation = TestLabel "Exponentiation tests" $ TestList
    [ binaryTest (^) (P.^) pos2 pos3
    , binaryTest (^) (P.^) zero pos5
    , binaryTest (^) (P.^) neg2 pos3
    , binaryTest (^) (P.^) pos5 zero
    , binaryTest (^) (P.^) zero zero
    , binaryTest (^) (P.^) neg5 zero
    ]

-- | Collect the test cases.
tests = TestList
    [ testAsIntegral
    , testIncrDecr
    , testNegate
    , testAbs
    , testSignum
    , testAddition
    , testSubtraction
    , testMultiplication
    , testDivision
    , testExponentiation
    ]

main = runTestTT tests
-- -}
