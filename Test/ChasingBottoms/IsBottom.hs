-- |
-- Module      :  Test.ChasingBottoms.IsBottom
-- Copyright   :  (c) Nils Anders Danielsson 2004-2007
-- License     :  See the file LICENCE.
-- 
-- Maintainer  :  http://www.cs.chalmers.se/~nad/
-- Stability   :  experimental
-- Portability :  non-portable (exceptions)
--

module Test.ChasingBottoms.IsBottom
  ( isBottom
  , bottom
  , nonBottomError
  , isBottomTimeOut
  ) where

import Prelude hiding (catch)
import Control.Exception (catch, throw, Exception(..), evaluate)
import System.IO.Unsafe (unsafePerformIO)
import qualified Test.ChasingBottoms.TimeOut as T

-- | @'isBottom' a@ returns 'False' if @a@ is distinct from bottom. If
-- @a@ equals bottom and results in an exception which is caught by
-- 'isBottom', and this exception is of a certain kind (see below),
-- then @'isBottom' a = 'True'@. Other caught exceptions are
-- re-thrown. If @a@ never reaches a weak head normal form and
-- never throws an exception, then @'isBottom' a@ never terminates.
--
-- The exceptions that yield 'True' are those that correspond to
-- \"pure bottoms\", i.e. bottoms that can originate in pure code.
-- Assertions are excluded, since their behaviour depends on compiler
-- flags (not pure, and a failed assertion should really yield an
-- exception and nothing else). The same applies to arithmetic
-- exceptions (machine dependent, except possibly for
-- 'Control.Exception.DivideByZero', but the value infinity makes that
-- case unclear as well).

-- Should we use throw or throwIO below?
--   It doesn't seem to matter, and I don't think it matters, but
--   using throw won't give us any problems.

-- Check out a discussion about evaluate around
-- http://www.haskell.org/pipermail/glasgow-haskell-users/2002-May/003393.html.

-- From the docs:
--   evaluate undefined `seq` return ()  ==> return ()
--   catch (evaluate undefined) (\e -> return ())  ==> return ()

isBottom :: a -> Bool
isBottom = isBottomTimeOut Nothing

-- | 'bottom' generates a bottom that is suitable for testing using
-- 'isBottom'.
bottom :: a
bottom = error "_|_"

-- | @'nonBottomError' s@ raises an exception ('AssertionFailed') that
-- is not caught by 'isBottom'. Use @s@ to describe the exception.

nonBottomError :: String -> a
nonBottomError = throw . AssertionFailed

-- | @'isBottomTimeOut' timeOutLimit@ works like 'isBottom', but if
-- @timeOutLimit@ is @'Just' lim@, then computations taking more than
-- @lim@ seconds are also considered to be equal to bottom. Note that
-- this is a very crude approximation of what a bottom is. Also note
-- that this \"function\" may return different answers upon different
-- invocations. Take it for what it is worth.
--
-- 'isBottomTimeOut' is subject to all the same scheduling vagaries as
-- 'Test.ChasingBottoms.TimeOut.timeOut'.

isBottomTimeOut :: Maybe Int -> a -> Bool
isBottomTimeOut timeOutLimit f = unsafePerformIO $
  maybeTimeOut (evaluate f) `catch` \e -> case e of
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
  where
  maybeTimeOut io = case timeOutLimit of
    Nothing -> do
      io
      return False
    Just lim -> do
      result <- T.timeOut lim io
      case result of               -- Note that evaluate bottom /= bottom.
        T.Value _        -> return False
        T.NonTermination -> return True
        T.Exception e    -> throw e  -- Catch the exception above.
