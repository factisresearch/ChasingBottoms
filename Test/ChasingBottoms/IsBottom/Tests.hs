-- | Tests of the functions in "Test.ChasingBottoms.IsBottom".
--
-- Note that the warnings given when compiling this module are
-- intentional. See the internal comments for more information.

module Test.ChasingBottoms.IsBottom.Tests (tests) where

import Test.ChasingBottoms.IsBottom
import System.IO.Unsafe
import Data.Array
import System.Exit
import qualified Control.Exception as E

isException f = unsafePerformIO $
  (E.evaluate f >> return False) `E.catch` const (return True)

bot = bot
notbot x = notbot x

data T' a = L' | B' (T' a) (T' a) deriving Eq

instance Monad T'

leftInfinite' = B' leftInfinite' L'

infiniteRecursion = leftInfinite' == leftInfinite'

data A2 = A2 { aaa :: A2 } | C { ccc :: A2 }

tests :: [Bool]
tests =
    -- Basic cases.
  [ isBottom bottom  ==  True
  , isBottom undefined  ==  True
  , isBottom (error "...")  ==  True
    -- This sometimes leads to a stack overflow.
    -- , isBottom bot  ==  True

    -- const bottom /= bottom.
  , isBottom notbot  ==  False
  , isBottom (const bottom)  ==  False

    -- Other types also lifted.
  , isBottom (bottom, bottom)  ==  False
  , isBottom (Just bottom)  ==  False

    -- Pattern match failure.
  , isBottom (let (x, y) = bottom in x :: Bool)  ==  True
  , isBottom (let Just x = Nothing in x :: Char)  ==  True

    -- Nonterminating, but not bottom.
  , isBottom [1..]  ==  False

    -- Missing methods.
    -- Skip this test to avoid compiler warnings.
  , (isBottom (L' >> L'))  ==  True

    -- Array stuff.
  , isBottom (array (1,0) [] ! 0)  ==  True
  , isBottom (array (0,0) [] ! 0)  ==  True

    -- Record stuff.
    -- Skip the first one to avoid compiler warnings.
  , isBottom (let x = A2 {} in aaa x)  ==  True
  , isBottom (let x = A2 { aaa = x } in ccc x)  ==  True
  , isBottom (let x = A2 { aaa = x } in x { ccc = x })  ==  True

    -- Infinite recursion, no data produced, should yield stack
    -- overflow...
    -- Not a quick test (on some machines, anyway). And the result
    -- might be optimisation dependent.
    -- , isException (isBottom infiniteRecursion)  ==  True

    -- Some other exceptions that are not caught, including
    -- nonBottomError.
  , isException (isBottom (unsafePerformIO $ exitWith ExitSuccess))  ==  True
  , isException (isBottom (1 `div` 0))  ==  True
  , isException (nonBottomError "...")  ==  True
  ]
