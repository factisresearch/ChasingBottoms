-- |
-- Module      :  ChasingBottoms.IsBottom
-- Copyright   :  (c) Nils Anders Danielsson 2004
-- License     :  See the file LICENSE.
-- 
-- Maintainer  :  http://www.cs.chalmers.se/~nad/
-- Stability   :  experimental
-- Portability :  non-portable (exceptions)
--

module ChasingBottoms.IsBottom(isBottom, bottom) where

import Prelude hiding (catch)
import Control.Exception (catch, throw, Exception(..), evaluate)
import System.IO.Unsafe (unsafePerformIO)

-- For testing purposes:
import System
import Array

-- | @'isBottom' a@ returns 'False' if @a@ is distinct from bottom. If
-- @a@ equals bottom and results in an exception which is caught by
-- 'isBottom', and this exception is of a certain kind (see below),
-- then @'isBottom' a = 'True'@. Other caught exceptions are
-- re-thrown. If @a@ never reaches a weak head normal form and
-- never throws an exception, then @'isBottom' a@ never terminates.
--
-- The exceptions that yield 'True' are those that correspond to \"pure
-- bottoms\", i.e. bottoms that can originate in pure code. Assertions
-- are excluded, since their behaviour depends on compiler flags (not
-- pure, and a failed assertion should really yield an exception and
-- nothing else). The same applies to arithmetic exceptions (machine
-- dependent, except possibly for 'DivideByZero', but the value
-- infinity makes that case unclear as well).

-- Should we use throw or throwIO below?
--   It doesn't seem to matter, and I don't think it matters, but
--   using throw won't give us any problems.

-- Check out a discussion about evaluate around
-- http://www.haskell.org/pipermail/glasgow-haskell-users/2002-May/003393.html.

-- From the docs:
--   evaluate undefined `seq` return ()  ==> return ()
--   catch (evaluate undefined) (\e -> return ())  ==> return ()

isBottom :: a -> Bool
isBottom f = unsafePerformIO $
  (evaluate f >> return False) `catch` \e -> case e of
    ArithException _   -> throw e
    ArrayException _   -> return True
    AssertionFailed _  -> throw e
    AsyncException _   -> throw e
    BlockedOnDeadMVar  -> throw e
    Deadlock           -> throw e
    DynException _     -> throw e
    ErrorCall _        -> return True
    ExitException _    -> throw e
    IOException _      -> throw e
    NoMethodError _    -> return True
    NonTermination     -> return True
    PatternMatchFail _ -> return True
    RecConError _      -> return True
    RecSelError _      -> return True
    RecUpdError _      -> return True

-- | 'bottom' generates a bottom that is suitable for testing using
-- 'isBottom'.
bottom :: a
bottom = error "_|_"


------------------------------------------------------------------------
-- Tests

isException f = unsafePerformIO $
  (f `seq` return False) `catch` const (return True)

bot = bot
notbot x = notbot x

data T a = L | B (T a) (T a) deriving Eq

-- instance Monad T where

leftInfinite = B leftInfinite L

infiniteRecursion = leftInfinite == leftInfinite

data A = A { aaa :: A } | C { ccc :: A }

tests = and
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
    -- , (isBottom (L >> L))  ==  True

    -- Array stuff.
  , isBottom (array (1,0) [] ! 0)  ==  True
  , isBottom (array (0,0) [] ! 0)  ==  True

    -- Record stuff.
    -- First one commented out to avoid compiler warnings.
    -- , isBottom (let x = A {} in aaa x)  ==  True
  , isBottom (let x = A { aaa = x } in ccc x)  ==  True
  , isBottom (let x = A { aaa = x } in x { ccc = x })  ==  True

    -- Infinite recursion, no data produced, should yield stack
    -- overflow...
    -- Not a quick test (on some machines, anyway). And the result
    -- might be optimisation dependent.
    -- , isException (isBottom infiniteRecursion)  ==  True

    -- Some other exceptions that are not caught.
  , isException (isBottom (unsafePerformIO $ exitWith ExitSuccess))  ==  True
  , isException (isBottom (1 `div` 0))  ==  True
  ]
