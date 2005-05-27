{-# OPTIONS -fth -cpp #-}

-- | By importing this module in e.g. "Test.ChasingBottoms" the
-- library refuses to be built if the tests fail.

module Test.ChasingBottoms.TestLibraryWhenCompiling () where

import qualified Test.ChasingBottoms.Tests as Tests
import qualified System.IO as IO
#if __GLASGOW_HASKELL__ <= 602
import qualified Language.Haskell.THSyntax as TH
#else
import qualified Language.Haskell.TH as TH
#endif

$(do 
#if __GLASGOW_HASKELL__ <= 602
     let runIO = TH.qIO
#else
     let runIO = TH.runIO
#endif
     b <- runIO $ do putStr "----- Testing the library. -----\n\n"
                     b <- Tests.main
                     IO.hFlush IO.stdout
                     return b
     if not b then
       fail "Tests failed. Refusing to build library."
      else
       return []
 )
