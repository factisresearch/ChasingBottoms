module NAD.LockFile
  ( lockFile
  -- , lockReadWrite
  , createLockFile
  , lockLockFile
  , createAndLock
  ) where

import Control.Monad
import System.IO
import System.Directory
import System.Posix.IO
import System.Posix.Types
import System.Posix.Unistd
import System.Posix.Files
-- import Control.Concurrent
import System.Random

-- | @'lockFile' fd comp@ blocks until a write lock has been aquired
-- for @fd@, then performs @comp@, and finally releases the lock.
--
-- Note that, unless mandatory locking is set on the file, other
-- processes are free to do whatever they want with it, except that
-- they cannot obtain a lock of their own.
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

-- | @'lockLockFile' file comp@ opens and locks @file@, runs @comp@,
-- closes @file@, and returns the result of @comp@. This can be useful
-- if @file@ is a lock file. If @file@ does not exist an exception is
-- thrown.

lockLockFile :: FilePath -> IO a -> IO a
lockLockFile file comp = do
  fd <- openFd file WriteOnly Nothing defaultFileFlags 
  a <- lockFile fd comp
  closeFd fd
  return a

-- | @'createLockFile' file@ does nothing if @file@ exists and
-- otherwise creates @file@ with the following permissions set:
--   * Owner write permission.
--   * Group write permission.

-- Using mandatory file locking does not seem to work on our Solaris
-- system. The files cannot even be opened...
--   * Mandatory file locking.

createLockFile :: FilePath -> IO ()
createLockFile file = do
  ex <- doesFileExist file
  unless ex $ do
    writeFile file ""
    setFileMode file (ownerWriteMode `unionFileModes` groupWriteMode)
    -- setFileMode file (setGroupIDMode `unionFileModes` ownerWriteMode)

-- | @'createAndLock' file comp@ first does a 'createLockFile' and
-- then a 'lockLockFile'.

createAndLock :: FilePath -> IO a -> IO a
createAndLock file comp = do
  createLockFile file
  lockLockFile file comp

-- | @'lockReadWrite' append file comp@ opens @file@, locks it, reads
-- its contents, runs @comp@ on the contents, writes the string
-- returned from @comp@ to the file, and finally unlocks the file. If
-- @append@ is 'True', then the output is appended to the original
-- contents of the file, and otherwise the original contents are
-- discarded.

-- lockReadWrite :: Bool -> FilePath -> (String -> IO String) -> IO ()
-- lockReadWrite append file comp = do
--   h <- openFile file ReadWriteMode
--   fd <- handleToFd h
--   lockFile fd $ do
--     s <- readEntireFile fd
--     s' <- comp s
--     when (not append) $ do
--       fdSeek fd AbsoluteSeek 0
--       -- Erase file.
--       ...
--     fdWrite fd s'
--     -- Handle possible errors.
--   closeFd fd

------------------------------------------------------------------------
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

main2 = do
  n <- randomRIO (0,100) :: IO Int
  createAndLock "apa.lock" $ do
    putStrLn $ "Got lock: " ++ show n
    sleep 1
    putStrLn $ "Releasing lock: " ++ show n
