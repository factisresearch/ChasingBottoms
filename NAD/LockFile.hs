module NAD.LockFile where

import System.IO
import System.Posix.IO
import System.Posix.Types
import System.Posix.Unistd
-- import Control.Concurrent

-- | @'lockFile' fd comp@ blocks until a write lock has been aquired
-- for @fd@, then performs @comp@, and finally releases the lock.
--
-- Note that other processes are free to do whatever they want with
-- the file, except that they cannot obtain a lock of their own.
--
-- The file descriptor must have been opened with write access,
-- otherwise an exception will occur. Locks are not inherited by
-- (bound) child processes.  If a signal is caught while the process
-- is blocked, this operation will result in an exception. If blocking
-- the process will lead to deadlock, this may lead to an exception.

lockFile :: Fd -> IO a -> IO a
lockFile fd comp = do
  waitToSetLock fd (WriteLock, AbsoluteSeek, 0, 0)
  a <- comp
  waitToSetLock fd (Unlock, AbsoluteSeek, 0, 0)
  return a

-- Tests.

main = do
  h <- openFile "NAD/test" AppendMode
  fd <- handleToFd h
  -- forkOS (doLock fd "child")  -- Not supported by default.
  doLock fd "parent"

doLock fd name =
  lockFile fd $ do
    putStrLn $ "Lock aquired: " ++ name
    sleep 2
    putStrLn $ "Lock just about to be released: " ++ name
