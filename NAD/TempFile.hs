{-# OPTIONS -fffi #-}

-- | Note that 'tempFileName' is recommended under Solaris, whereas
-- 'System.Posix.mkstemp' is recommended under Linux. Remember to set
-- the umask in a suitable way before running these functions.

module NAD.TempFile
  ( tempFile
  , tempFilename
  ) where

import IO
import Monad
import System.Posix
import Foreign
import Foreign.C

main = do
  n <- tempFilename Nothing Nothing
  print n
  setEnv "TMPDIR" "/users/cs/nad/temp/" False
  n' <- tempFilename Nothing (Just "bubba")
  print n'
  n'' <- tempFilename (Just ".") (Just "bubba")
  print n''
  h <- tempFile
  hPutStrLn h "Testing 1 2 3"
  hGetContents h >>= putStr
  hClose h

-- tempFile returns a handle to a temporary file that is deleted when
-- the handle is closed. Note that the function may throw exceptions
-- (IOErrors).

tempFile :: IO Handle
tempFile = do
  filePtr <- c_tmpfile
  if filePtr == nullPtr then
    throwErrno "tempFile"
   else do
    fd <- c_fileno filePtr
    fdToHandle fd

-- tempFilename maybe returns a string that can be used as a temporary
-- name. The directory for the file can be given and a template (of up
-- to five characters) can be used for the filename. The function
-- doesn't guarantee that no other application opens the particular
-- returned name. Using a template ought to reduce this risk.

tempFilename :: Maybe String -> Maybe String -> IO (Maybe String)
tempFilename mDir mTemplate =
  mWithCString (fmap (take 5) mTemplate) $ \template ->
    mWithCString mDir $ \dir -> do
      name <- c_tempnam dir template
      if name == nullPtr then
        return Nothing
       else do
        tempName <- peekCString name
        free name
        return $ Just tempName
  where
  mWithCString ms io = case ms of
    Nothing -> io nullPtr
    Just s -> withCString s io

foreign import ccall "stdio.h tempnam" c_tempnam ::
  CString -> CString -> IO CString

foreign import ccall "stdio.h tmpfile" c_tmpfile :: IO (Ptr CFile)

foreign import ccall "stdio.h fileno" c_fileno :: Ptr CFile -> IO Fd
