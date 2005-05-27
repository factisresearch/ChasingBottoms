{-# OPTIONS -fglasgow-exts -cpp #-}

-- | Tests of the functions in "Test.ChasingBottoms.Nat".

module Test.ChasingBottoms.Nat.Tests (tests) where

import Test.ChasingBottoms.Nat
#if __GLASGOW_HASKELL__ <= 602
import Debug.QuickCheck
import Debug.QuickCheck.Batch
#else
import Test.QuickCheck
import Test.QuickCheck.Batch
#endif
import Data.List
import Data.Ratio
import Text.Show.Functions

-- Testing isSucc.

prop_isSucc n = isSucc n == (n > 0)

-- Testing fromSucc.

prop_fromSucc n | n == 0    = fromSucc n == Nothing
                | otherwise = fromSucc n == Just (n-1)

-- Testing natrec.

-- How do you test something as versatile as natrec? Well, at least we
-- can verify that we can use it to implement addition.

prop_natrec_add m n = natrec m (\_ o -> succ o) n == m + n

-- There is no need to test foldN, since it is specified by its
-- definition.

-- Testing Enum.

prop_Nat_Enum_succ (n :: Nat) = succ n == n + 1
prop_Nat_Enum_pred (n :: Nat) = n > 0 ==> pred n == n - 1

-- Testing Eq.

prop_Nat_Eq_refl (m :: Nat) = m == m
prop_Nat_Eq_sym (m :: Nat) n =
  collect (m, n) $  -- OK distribution.
    m == n ==> n == m
prop_Nat_Eq_trans (i :: Nat) j k =
  collect (i, j, k) $  -- Distribution good enough.
    i == j && j == k ==> i == k
prop_Nat_Eq_cong (f :: Nat -> Nat) m n =
  collect (m, n) $  -- OK distribution.
    m == n ==> f m == f n

prop_Nat_Eq_noteq (m :: Nat) n = (m == n) == not (m /= n)

-- Testing Show.

prop_Nat_Show (m :: Nat) = show m == show (toInteger m)

-- Testing Ord.

prop_Nat_Ord_refl (m :: Nat) = m <= m
prop_Nat_Ord_antisym (m :: Nat) n =
  collect (m, n) $  -- OK distribution.
    m <= n && n <= m ==> m == n
prop_Nat_Ord_trans (i :: Nat) j k =
  collect (i, j, k) $  -- OK distribution.
    i <= j && j <= k ==> i <= k

-- Testing Num.

prop_Nat_mul_iterated_sum (m :: Nat) n =
  m * n == foldr (+) 0 (genericReplicate m n)

prop_Nat_plus_assoc (m :: Nat) n o = m + (n + o) == (m + n) + o
prop_Nat_plus_comm (m :: Nat) n    = m + n == n + m

prop_Nat_mul_assoc (m :: Nat) n o = m * (n * o) == (m * n) * o
prop_Nat_mul_comm (m :: Nat) n    = m * n == n * m

prop_Nat_mul_plus_left_dist (m :: Nat) n o = m * (n + o) == m * n + m * o

prop_Nat_mul_plus_zero (m :: Nat) = m + 0 == m
prop_Nat_mul_mul_unit (m :: Nat)  = m * 1 == m

prop_Nat_minus (m :: Nat) n =
  collect (m, n) $  -- OK distribution.
  m >= n ==> (m - n) + n == m

prop_Nat_signum_abs (m :: Nat) = signum m * abs m == m
prop_Nat_signum_zero           = signum 0 == 0

prop_Nat_fromInteger_plus m n =
  m >= 0 && n >= 0 ==>
    fromInteger m + fromInteger n == (fromInteger (m + n) :: Nat)
prop_Nat_fromInteger_mul m n =
  m >= 0 && n >= 0 ==>
    fromInteger m * fromInteger n == (fromInteger (m * n) :: Nat)

-- negate is undefined.

-- Testing Integral.

prop_Nat_to_from (m :: Nat) = fromInteger (toInteger m) == m
prop_Nat_from_to i = i >= 0 ==> toInteger (fromInteger i :: Nat) == i

prop_Nat_quotRem (m :: Nat) n =
  n /= 0 ==> m `quotRem` n == (m `quot` n, m `rem` n)
prop_Nat_divMod (m :: Nat) n =
  n /= 0 ==> m `divMod` n == (m `div` n, m `mod` n)

prop_Nat_quot_rem (m :: Nat) n =
  n /= 0 ==> (m `quot` n) * n + m `rem` n == m
prop_Nat_div_mod (m :: Nat) n =
  n /= 0 ==> (m `div` n) * n + m `mod` n == m

-- Testing Real.

prop_Nat_toRational (m :: Nat) = toRational m == toInteger m % 1

-- | All tests collected together.

tests = do
  results <- mapM ($ testOptions) theTests
  mapM_ (putStr . show) results
  return $ all ok $ results
  where
  ok (TestOk {})       = True
  ok (TestExausted {}) = True   -- We treat this as OK since the
                                -- distribution of test data is displayed.
  ok (TestFailed {})   = False
  ok (TestAborted {})  = False

  testOptions = TestOptions { no_of_tests     = 1000
                            , length_of_tests = 0      -- No time limit.
                            , debug_tests     = False
                            }

  theTests =
    [ run prop_isSucc
    , run prop_fromSucc
    , run prop_natrec_add
    , run prop_Nat_Enum_succ
    , run prop_Nat_Enum_pred
    , run prop_Nat_Eq_refl
    , run prop_Nat_Eq_sym
    , run prop_Nat_Eq_trans
    , run prop_Nat_Eq_cong
    , run prop_Nat_Eq_noteq
    , run prop_Nat_Show
    , run prop_Nat_Ord_refl
    , run prop_Nat_Ord_antisym
    , run prop_Nat_Ord_trans
    , run prop_Nat_mul_iterated_sum
    , run prop_Nat_plus_assoc
    , run prop_Nat_plus_comm
    , run prop_Nat_mul_assoc
    , run prop_Nat_mul_comm
    , run prop_Nat_mul_plus_left_dist
    , run prop_Nat_mul_plus_zero
    , run prop_Nat_mul_mul_unit
    , run prop_Nat_minus
    , run prop_Nat_signum_abs
    , run prop_Nat_signum_zero
    , run prop_Nat_fromInteger_plus
    , run prop_Nat_fromInteger_mul
    , run prop_Nat_to_from
    , run prop_Nat_from_to
    , run prop_Nat_quotRem
    , run prop_Nat_divMod
    , run prop_Nat_quot_rem
    , run prop_Nat_div_mod
    , run prop_Nat_toRational
    ]

-- | Show instance for 'TestResult' suitable for the tests run above.

instance Show TestResult where
  show (TestOk _ n args) =
    "OK, passed " ++ show n ++ " tests.\n" ++ showArgs args
  show (TestExausted _ n args) =
    "Arguments exhausted after " ++ show n ++ " tests.\n" ++ showArgs args
  show (TestFailed _ _) = "Test failed.\n"
  show (TestAborted _) = "Test resulted in exception.\n"

-- | Helper function for the 'TestResult' 'Show' instance.

showArgs :: [[String]] -> String
showArgs args
  | all null args = ""
  | otherwise     = unlines . map (indent . concat . intersperse ", ") $ args
  where indent = ("  " ++)
