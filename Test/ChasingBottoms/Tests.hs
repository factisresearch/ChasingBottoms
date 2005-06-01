{-# OPTIONS -fglasgow-exts -cpp #-}

-- | Tests of almost everything related to "Test.ChasingBottoms".

module Test.ChasingBottoms.Tests (main) where

import qualified Test.ChasingBottoms.Approx.Tests      as Approx
import qualified Test.ChasingBottoms.ApproxShow.Tests  as ApproxShow
import qualified Test.ChasingBottoms.IsBottom.Tests    as IsBottom
import qualified Test.ChasingBottoms.IsType.Tests      as IsType
import qualified Test.ChasingBottoms.Nat.Tests         as Nat
import qualified Test.ChasingBottoms.SemanticOrd.Tests as SemanticOrd
import qualified Test.ChasingBottoms.TimeOut.Tests     as TimeOut

-- | A class for things that can be tested.
class Test a where
  test :: String      -- ^ Description of test.
          -> a        -- ^ Test.
          -> IO Bool  -- ^ True if the test succeeded.

-- | @'indent' a@ shows @a@ and indents the output by two spaces. A
-- trailing newline is added if necessary.

-- This function could be more efficient.

indent :: (Show a) => a -> IO ()
indent a = putStr . maybeNL . unlines . map ("  " ++) . lines $ show a
  where maybeNL s | null s         = "\n"
                  | last s == '\n' = s
                  | otherwise      = s ++ "\n"

instance Test Bool where
  test desc b = do
    putStrLn desc
    indent b
    return b

instance Test [Bool] where
  test desc bs = do
    putStrLn desc
    indent bs
    return $ and bs

instance Test (IO Bool) where
  test desc io = do
    putStrLn desc
    b <- io
    indent b
    return b

-- | This function runs all the tests, and returns 'True' if all of
-- them succeeded.

main :: IO Bool
main = do
  ok <- fmap and $ sequence theTests
  putStrLn ""
  if ok then
    putStrLn "All tests succeeded."
   else
    putStrLn "At least one test failed."
  return ok
  where theTests = [ test "Approx:"      Approx.tests
                   , test "ApproxShow:"  ApproxShow.tests
                   , test "IsBottom:"    IsBottom.tests
                   , test "IsType:"      IsType.tests
                   , test "Nat:"         Nat.tests
                   , test "SemanticOrd:" SemanticOrd.tests
                   , test "TimeOut:"     TimeOut.tests
                   ]
