-- | Code stolen from the QuickCheck sources and modified slightly.

module ChasingBottoms.QuickCheckWrapper where

import Debug.QuickCheck hiding (check)
import qualified Random
import qualified System.IO.Unsafe as Unsafe

check :: Testable a => Config -> a -> IO Bool
check config a =
  do rnd <- Random.newStdGen
     tests config (evaluate a) rnd 0 0 []
  where
  tests :: Monad m => Config -> Gen Result -> Random.StdGen
                   -> Int -> Int -> [[String]] -> m Bool
  tests config gen rnd0 ntest nfail stamps
    | ntest == configMaxTest config = return True
    | nfail == configMaxFail config = fail "Too many failing test cases."
    | otherwise               =
        do case ok result of
             Nothing    ->
               tests config gen rnd1 ntest (nfail+1) stamps
             Just True  ->
               tests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
             Just False ->
               return False
       where
        result      = generate (configSize config ntest) rnd2 gen
        (rnd1,rnd2) = Random.split rnd0

testIt :: Property -> Bool
testIt = Unsafe.unsafePerformIO . check config
  where
  config :: Config
  config = Config
    { configMaxTest = 100
    , configMaxFail = 1000
    , configSize    = (+ 3) . (`div` 2)
    , configEvery   = const (const "")
    }
