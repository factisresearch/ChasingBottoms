{-# OPTIONS -cpp #-}

-- | Some utilities that are part of the testing framework.

module Test.ChasingBottoms.TestUtilities (runQuickCheckTests) where

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
