-- |
-- Module      :  Test.ChasingBottoms
-- Copyright   :  (c) Nils Anders Danielsson 2004-2007
-- License     :  See the file LICENCE.
-- 
-- Maintainer  :  http://www.cs.chalmers.se/~nad/
-- Stability   :  experimental
-- Portability :  non-portable (GHC-specific)
--
-- This module just re-exports all the other modules.

module Test.ChasingBottoms
  ( module Test.ChasingBottoms.Approx
  , module Test.ChasingBottoms.ApproxShow
  , module Test.ChasingBottoms.ContinuousFunctions
  , module Test.ChasingBottoms.IsBottom
  , module Test.ChasingBottoms.Nat
  , module Test.ChasingBottoms.SemanticOrd
  , module Test.ChasingBottoms.TimeOut
  ) where

import Test.ChasingBottoms.Approx
import Test.ChasingBottoms.ApproxShow
import Test.ChasingBottoms.ContinuousFunctions
import Test.ChasingBottoms.IsBottom
import Test.ChasingBottoms.Nat
import Test.ChasingBottoms.SemanticOrd
import Test.ChasingBottoms.TimeOut

{-

-- If this module is imported the library refuses to be built whenever
-- the tests fail. Doing this adds a dependency on Template Haskell,
-- though, and the compilation takes considerably longer, plus some
-- (intended) warnings are emitted. Furthermore, if some test should
-- fail, then the emitted error message is rather unreadable.

import Test.ChasingBottoms.TestLibraryWhenCompiling

-}
