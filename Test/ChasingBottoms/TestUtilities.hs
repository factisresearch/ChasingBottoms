{-# OPTIONS -cpp #-}

-- | Some utilities that are part of the testing framework.

module Test.ChasingBottoms.TestUtilities
  ( -- * Batch execution of QuickCheck tests
    runQuickCheckTests
    -- * Various algebraic properties
    -- ** Equivalence and congruence
  , isEquivalenceRelation
  , isCongruence
  , eqIsCongruence
    -- ** Partial and total orders
  , isPartialOrder
  , isTotalOrder
  , isPartialOrderOperators
  , isTotalOrderOperators
  , ordIsTotalOrder
  ) where

#if __GLASGOW_HASKELL__ <= 602
import Debug.QuickCheck
import Debug.QuickCheck.Batch
#else
import Test.QuickCheck
import Test.QuickCheck.Batch
#endif
import Data.List
import Text.Show.Functions

------------------------------------------------------------------------
-- Batch execution of QuickCheck tests

-- | Runs a bunch of QuickCheck tests, printing suitable information
-- to standard output. Returns 'True' if no tests fail. Note that a
-- test where the inputs are exhausted is considered to have
-- succeeded.

runQuickCheckTests :: [TestOptions -> IO TestResult]
                      -- ^ Create the tests in this list from ordinary
                      -- QuickCheck tests by using 'run'.
                      -> IO Bool
runQuickCheckTests tests = do
  results <- mapM ($ testOptions) tests
  mapM_ (putStr . showTR) results
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

  showTR (TestOk _ n args) =
    "OK, passed " ++ show n ++ " tests.\n" ++ showArgs args
  showTR (TestExausted _ n args) =
    "Arguments exhausted after " ++ show n ++ " tests.\n" ++ showArgs args
  showTR (TestFailed _ _) = "Test failed.\n"
  showTR (TestAborted _) = "Test resulted in exception.\n"

  showArgs :: [[String]] -> String
  showArgs args
    | all null args = ""
    | otherwise     = unlines . map (indent . concat . intersperse ", ") $ args
    where indent = ("  " ++)

------------------------------------------------------------------------
-- Testing various algebraic properties

-- | Tests for an equivalence relation. Requires that the relation is
-- neither always false nor always true.

isEquivalenceRelation
  :: Show a
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element equivalent to argument.
     -> (a -> Gen a)
     -- ^ Generator for element not equivalent to argument.
     -> (a -> a -> Bool)
     -- ^ The relation.
     -> [Property]
isEquivalenceRelation element equalTo notEqualTo (===) =
  [reflexive, symmetric1, symmetric2, transitive]
  where
  x /== y = not (x === y)

  reflexive = forAll element $ \x ->
                x === x

  symmetric1 = forAll (pair element equalTo) $ \(x, y) ->
                 x === y && y === x

  symmetric2 = forAll (pair element notEqualTo) $ \(x, y) ->
                 x /== y && y /== x

  transitive = forAll (pair element equalTo) $ \(x, y) ->
                 forAll (equalTo y) $ \z ->
                   x === z


-- | Tests for a congruence. 'Arbitrary' instance needed for
-- 'coarbitrary'.

isCongruence
  :: (Show a, Arbitrary a)
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element equivalent to argument.
     -> (a -> Gen a)
     -- ^ Generator for element not equivalent to argument.
     -> (a -> a -> Bool)
     -- ^ The relation.
     -> [Property]
isCongruence element equalTo notEqualTo (===) =
  isEquivalenceRelation element equalTo notEqualTo (===) ++ [cong]
  where
  cong = forAll arbitrary $ \f ->
           forAll (pair element equalTo) $ \(x, y) ->
             f x == (f y :: Integer)

-- | Test that an 'Eq' instance is a congruence, and that '/=' is
-- the negation of '=='.

eqIsCongruence
  :: (Show a, Arbitrary a, Eq a)
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element equivalent to argument.
     -> (a -> Gen a)
     -- ^ Generator for element not equivalent to argument.
     -> [Property]
eqIsCongruence element equalTo notEqualTo =
  isCongruence element equalTo notEqualTo (==) ++ [eq_neq1, eq_neq2]
  where
  eq_neq1 = forAll (pair element equalTo) $ \(x, y) ->
              x == y && not (x /= y)
  eq_neq2 = forAll (pair element notEqualTo) $ \(x, y) ->
              not (x == y) && x /= y

-- | Tests for a partial order.

isPartialOrder
  :: Show a
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element equal to argument, according to
     -- underlying equality relation.
     -> (a -> Gen a)
     -- ^ Generator for element different from argument, according to
     -- underlying equality relation.
     -> (a -> Gen a)
     -- ^ Generator for element greater than or equal to argument.
     -> (a -> a -> Bool)
     -- ^ Underlying equality relation.
     -> (a -> a -> Bool)
     -- ^ The relation.
     -> [Property]
isPartialOrder element equalTo differentFrom greaterThan (==.) (<=.) =
  [reflexive, antisymmetric1, antisymmetric2, transitive]
  where
  infix 4 ==., <=.

  reflexive =
    forAll element $ \x ->
      x <=. x

  antisymmetric1 =
    forAll (pair element equalTo) $ \(x, y) ->
      (x <=. y && y <=. x) && x ==. y

  antisymmetric2 =
    forAll (pair element differentFrom) $ \(x, y) ->
      not (x <=. y && y <=. x) && not (x ==. y)

  transitive = forAll (pair element greaterThan) $ \(x, y) ->
                 forAll (greaterThan y) $ \z ->
                   x <=. z

-- | Tests for a total order.

isTotalOrder
  :: Show a
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element equal to argument, according to
     -- underlying equality relation.
     -> (a -> Gen a)
     -- ^ Generator for element different from argument, according to
     -- underlying equality relation.
     -> (a -> Gen a)
     -- ^ Generator for element greater than or equal to argument.
     -> (a -> a -> Bool)
     -- ^ Underlying equality relation.
     -> (a -> a -> Bool)
     -- ^ The relation.
     -> [Property]
isTotalOrder element equalTo differentFrom greaterThan (==.) (<=.) =
  isPartialOrder element equalTo differentFrom greaterThan (==.) (<=.)
  ++ [total]
  where
  infix 4 <=.

  total =
    forAll element $ \x ->
    forAll element $ \y ->
      x <=. y || y <=. x

-- | Tests relating various partial order operators. Does not include
-- any tests from 'isPartialOrder'.

isPartialOrderOperators
  :: Show a
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> a -> Bool)
     -- ^ Equal.
     -> (a -> a -> Bool)
     -- ^ Less than or equal.
     -> (a -> a -> Bool)
     -- ^ Less than.
     -> (a -> a -> Bool)
     -- ^ Greater than or equal.
     -> (a -> a -> Bool)
     -- ^ Greater than.
     -> [Property]
isPartialOrderOperators element (==.) (<=.) (<.) (>=.) (>.) =
  [lt_le, gt_le, ge_lt]
  where
  infix 4 ==., <=., <., >=., >.

  lt_le =
    forAll element $ \x ->
    forAll element $ \y ->
      (x <. y) == (x <=. y && not (x ==. y))

  gt_le =
    forAll element $ \x ->
    forAll element $ \y ->
      (x >. y) == not (x <=. y)

  ge_lt =
    forAll element $ \x ->
    forAll element $ \y ->
      (x >=. y) == not (x <. y)


-- | Tests relating various total order operators and functions. Does
-- not include any tests from 'isTotalOrder'.

isTotalOrderOperators
  :: Show a
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> a -> Bool)
     -- ^ Equal.
     -> (a -> a -> Bool)
     -- ^ Less than or equal.
     -> (a -> a -> Bool)
     -- ^ Less than.
     -> (a -> a -> Bool)
     -- ^ Greater than or equal.
     -> (a -> a -> Bool)
     -- ^ Greater than.
     -> (a -> a -> Ordering)
     -- ^ Compare.
     -> (a -> a -> a)
     -- ^ Minimum.
     -> (a -> a -> a)
     -- ^ Maximum.
     -> [Property]
isTotalOrderOperators element (==.) (<=.) (<.) (>=.) (>.) cmp mn mx =
  isPartialOrderOperators element (==.) (<=.) (<.) (>=.) (>.)
  ++ [compare_lt_eq_gt, compare_max, compare_min]
  where
  compare_lt_eq_gt =
    forAll element $ \x ->
    forAll element $ \y ->
      case cmp x y of
        LT -> x <. y
        EQ -> x ==. y
        GT -> x >. y

  compare_max =
    forAll element $ \x ->
    forAll element $ \y ->
      case cmp x y of
        LT -> x `mx` y ==. y
        GT -> x `mx` y ==. x
        EQ -> elemBy (==.) (x `mx` y) [x, y]

  compare_min =
    forAll element $ \x ->
    forAll element $ \y ->
      case cmp x y of
        LT -> x `mn` y ==. x
        GT -> x `mn` y ==. y
        EQ -> elemBy (==.) (x `mn` y) [x, y]

  elemBy op x xs = any (`op` x) xs

-- | Tests that an 'Ord' instance should satisfy to be a total order.

ordIsTotalOrder
  :: (Show a, Ord a)
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element equal to argument.
     -> (a -> Gen a)
     -- ^ Generator for element different from argument.
     -> (a -> Gen a)
     -- ^ Generator for element greater than or equal to argument.
     -> [Property]
ordIsTotalOrder element equalTo differentFrom greaterThan =
  isTotalOrderOperators element (==) (<=) (<) (>=) (>) compare min max
  ++ isTotalOrder element equalTo differentFrom greaterThan (==) (<=)

------------------------------------------------------------------------
-- Local helper functions

-- | Given two generators, generates a pair where the second component
-- depends on the first.

pair :: Gen a -> (a -> Gen b) -> Gen (a, b)
pair gen1 gen2 = do
  x <- gen1
  y <- gen2 x
  return (x, y)

