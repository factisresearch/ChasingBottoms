{-# OPTIONS -fth #-}

-- | By importing this module in e.g. "Test.ChasingBottoms" the
-- library refuses to be built if the tests fail.

module Test.ChasingBottoms.TestLibraryWhenCompiling () where

import qualified Test.ChasingBottoms.Tests as Tests
import qualified System.IO as IO
import qualified Language.Haskell.TH as TH

$(do 
     b <- TH.runIO $ do putStr "----- Testing the library. -----\n\n"
                        b <- Tests.main
                        IO.hFlush IO.stdout
                        return b
     if not b then
       fail "Tests failed. Refusing to build library."
      else
       return []
 )
