{-# OPTIONS -fglasgow-exts #-}

-- | When dealing with \"hard bottoms\", i.e. non-terminating
-- computations that do not result in exceptions, the following functions
-- may be handy.

module ChasingBottoms.TimeOut( timeOut, timeOutMicro ) where

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
