{-# OPTIONS -fglasgow-exts #-}

-- |
-- Module      :  ChasingBottoms.TimeOut
-- Copyright   :  (c) Nils Anders Danielsson 2004
-- License     :  See the file LICENSE.
-- 
-- Maintainer  :  http://www.cs.chalmers.se/~nad/
-- Stability   :  experimental
-- Portability :  non-portable (preemptive scheduling)
--
-- When dealing with \"hard bottoms\", i.e. non-terminating
-- computations that do not result in exceptions, the following functions
-- may be handy.
--
-- Note that a computation is considered to have terminated when it
-- has reached weak head normal form (i.e. something distinct from
-- bottom).

module ChasingBottoms.TimeOut
  ( timeOut
  , timeOut'
  , timeOutMicro
  , timeOutMicro'
  ) where

import Control.Concurrent
import Control.Exception
import Data.Typeable

data WakeUp = WakeUp deriving Typeable

-- | @'timeOut' n c@ runs @c@ for at most @n@ seconds (modulo
-- scheduling issues). If the computation terminates before that, then
-- the resulting value is returned, and otherwise 'Nothing' is returned.
timeOut :: Int -> IO a -> IO (Maybe a)
timeOut = timeOutMicro . (* 10^6)

-- | 'timeOutMicro' takes a delay in microseconds. Note that the
-- resolution is not necessarily very high (the last time I checked it
-- was 0.02 seconds).
timeOutMicro :: Int -> IO a -> IO (Maybe a)
timeOutMicro delay io = do
  id <- myThreadId
  mv <- newEmptyMVar
  ioThread <- forkIO $ do
    a <- io
    putMVar mv a
  reaper <- forkIO $ do
    threadDelay delay
    throwDynTo id WakeUp
  (fmap Just (takeMVar mv) `catchDyn` \WakeUp -> do
    killThread ioThread
    return Nothing)
    `finally` killThread reaper

-- | 'timeOut'' is a variant which can be used for pure
-- computations. The definition,
--
-- @
--   'timeOut'' n = 'timeOut' n . Control.Exception.evaluate
-- @
--
-- ensures that @'timeOut'' 1 bottom@ usually returns 'Nothing'.
-- (@'timeOut' 1 (return bottom)@ usually returns @'Just' 'bottom'@.)
timeOut' :: Int -> a -> IO (Maybe a)
timeOut' n = timeOut n . evaluate

-- | 'timeOutMicro'' is the equivalent variant of 'timeOutMicro':
--
-- @
--  'timeOutMicro'' n = 'timeOutMicro' n . Control.Exception.evaluate
-- @
timeOutMicro' :: Int -> a -> IO (Maybe a)
timeOutMicro' n = timeOutMicro n . evaluate
