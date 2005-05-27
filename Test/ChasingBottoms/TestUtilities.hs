{-# OPTIONS -cpp #-}

-- | Some utilities that are part of the testing framework.

module Test.ChasingBottoms.TestUtilities
  ( runQuickCheckTests
  , prop_equivalence_relation
  , prop_congruence
  , prop_Eq_congruence
  ) where

#if __GLASGOW_HASKELL__ <= 602
import Debug.QuickCheck
import Debug.QuickCheck.Batch
#else
import Test.QuickCheck
import Test.QuickCheck.Batch
#endif
import Data.List

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

prop_equivalence_relation
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
prop_equivalence_relation element equalTo notEqualTo (===) =
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

prop_congruence
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
prop_congruence element equalTo notEqualTo (===) =
  prop_equivalence_relation element equalTo notEqualTo (===) ++ [cong]
  where
  cong = forAll arbitrary $ \f ->
           forAll (pair element equalTo) $ \(x, y) ->
             f x == (f y :: Integer)

-- | Test that an 'Eq' instance is a congruence, and that '(/=)' is
-- the negation of '(==)'.

prop_Eq_congruence
  :: (Show a, Arbitrary a, Eq a)
     => Gen a
     -- ^ Generator for arbitrary element.
     -> (a -> Gen a)
     -- ^ Generator for element equivalent to argument.
     -> (a -> Gen a)
     -- ^ Generator for element not equivalent to argument.
     -> [Property]
prop_Eq_congruence element equalTo notEqualTo =
  prop_congruence element equalTo notEqualTo (==) ++ [eq_neq1, eq_neq2]
  where
  eq_neq1 = forAll (pair element equalTo) $ \(x, y) ->
              x == y && not (x /= y)
  eq_neq2 = forAll (pair element notEqualTo) $ \(x, y) ->
              not (x == y) && x /= y

------------------------------------------------------------------------
-- Local helper functions

-- | Given two generators, generates a pair where the second component
-- depends on the first.

pair :: Gen a -> (a -> Gen b) -> Gen (a, b)
pair gen1 gen2 = do
  x <- gen1
  y <- gen2 x
  return (x, y)

