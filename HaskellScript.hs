module Main where

import System.Posix (getEnvDefault, createPipe, fdToHandle, closeFd,
                     getProcessStatus, forkProcess, dupTo, executeFile,
                     stdInput, ProcessID, Fd, signalProcess, installHandler,
                     Handler(..), sigINT, sigCHLD, ProcessStatus(..),
                     exitImmediately)
import Control.Exception (handleJust, ioErrors, throwIO, block, unblock,
                          Exception(..))
import Foreign.C (throwErrno, getErrno, eINTR)
import System (getArgs, getProgName, exitFailure, exitWith, ExitCode(..))
import IO (hPutStrLn, hPutStr, hPutChar, hFlush, hClose, isEOF, stderr,
           catch, hWaitForInput, stdin, isEOFError)
import Monad (when)
import Text.PrettyPrint.HughesPJ (render, text, nest, ($$), (<+>))

ghciDefault = "/users/cs/group_multi/pub/bin/ghci"

errorCode = ExitFailure 1
errorCodeStr = "System.ExitFailure 1"

main = do
  args <- getArgs
  progName <- getProgName
  when (null args) $ errorExit $ usage progName
  ghci <- getEnvDefault "GHCI" ghciDefault
  extraGhciOpts <- getEnvDefault "GHCIOPTS" ""
  let
    haskellProg = head args
    extraArgs = tail args

    ghciOpts = ["-v0", "-F", "-pgmF", preprocessor, haskellProg]
               ++ words extraGhciOpts
    preprocessor = "haskell-script-helper"

    -- The preprocessor has to reside on the path, and look
    -- somewhat like the following:
    --   #!/bin/sh
    --   grep -v '^#!' $2 > $3
    -- Reason: ghci doesn't understand the #!... line.

    setProgNameCommand = unwords [":set prog", haskellProg]
    setArgsCommand = if null extraArgs then
                       []
                      else
                       [unwords $ ":set args" : extraArgs]
    -- The following command ensures that whatever is left of stdin is
    -- discarded after running the main function.
    mainCommand = concat
      [ "Control.Exception.block $ do"
      , "{ eExc <- Control.Exception.try $ Control.Exception.unblock main"
      , "; case eExc of"
      , "    Right _ -> System.Posix.exitImmediately System.ExitSuccess"
      , ";   Left (Control.Exception.ExitException e) ->"
      , "      System.Posix.exitImmediately e"
      , ";   Left e -> do"
      , "      { print e"
      , "      ; System.Posix.exitImmediately (" ++ errorCodeStr ++ ")"
      , "      }"
      , "}"
      ]
    ghciCmds = unlines $ setArgsCommand ++ [setProgNameCommand, mainCommand]

  (readEnd, writeEnd) <- createPipe
  writeEndH <- fdToHandle writeEnd

  hPutStr writeEndH $ ghciCmds
  hFlush writeEndH

  -- Maybe we should block sigINT and sigCHLD while the other
  -- processes are started, before the signal handlers are
  -- installed. But the signals shouldn't be blocked in the other
  -- process, should they? Unblocking after fork doesn't seem to
  -- work... Using Haskell's block and unblock doesn't cause problems,
  -- but does it solve them?

  -- Start ghci.
  ghciPid <- block $ do
    ghciPid <- runProcessWithInputAndExtra False ghci ghciOpts readEnd $
                 \io -> unblock (do hClose writeEndH; io)

    -- Die when ghci dies.
    installHandler sigCHLD (Catch $ do
        mStatus <- getProcessStatus False False ghciPid
        case mStatus of
          Just (Stopped {}) -> return ()  -- Ignore these messages.
          Just (Exited exitCode) -> do
            exitImmediately exitCode
          _ -> do
            exitImmediately errorCode
      ) Nothing

    -- Make sure that ^C kills also ghci.
    installHandler sigINT (Catch $ do
        signalProcess sigINT ghciPid
        exitFailure
      ) Nothing

    return ghciPid

  closeFd readEnd
  forwardStdin writeEndH
  hClose writeEndH

  -- Catch exceptions raising from the fact that ghci is killed (^C).
  -- (getProcessStatus throws an exception when it is interrupted.)
  let catchEINTR e = do
       errno <- getErrno
       when (errno /= eINTR) $ throwIO (IOException e)
  handleJust ioErrors catchEINTR $ do
    -- Wait for ghci.
    getProcessStatus True False ghciPid
    return ()

-- This function can probably be improved. In effect it uses non-buffering IO.
forwardStdin outH = do
  eof <- isEOF
  when (not eof) $ do
    c <- getChar
    hPutChar outH c
    hFlush outH
    forwardStdin outH

runProcessWithInputAndExtra :: Bool -> String -> [String] -> Fd
                               -> (IO () -> IO ()) -> IO ProcessID
runProcessWithInputAndExtra useSearchPath prog args fd extra =
  forkIO' $ extra $ do
    dupTo fd stdInput
    when (fd /= stdInput) $ closeFd fd
    executeFile prog useSearchPath args Nothing
    -- Only happens when executeFile fails.
    throwErrno "runProcessWithInputAndExtra"

forkIO' :: IO () -> IO ProcessID
forkIO' io = do
  mPid <- forkProcess
  case mPid of
    Nothing -> do
      io
      exitWith ExitSuccess
    Just pid ->
      if pid == -1 then
        throwErrno "forkIO'"
       else
        return pid

usage progName = render $
  text "Usage:" <+> text progName <+> text "program [args...]" $$
  text "" $$
  text "Runs the Haskell source file <program> with command line arguments" $$
  text "<args> using ghci. Note that ghci parses the arguments in a rather" $$
  text "crude way, so don't expect \", \', escaped spaces etc. to work." $$
  text "The location of ghci is determined by the GHCI environment variable." $$
  text "The default is:" $$ nest 2 (text ghciDefault) $$
  text "Extra options to ghci can be given in the GHCIOPTS environment variable." $$
  text "Note that the contents of this variable is cut up into words along" $$
  text "whitespace boundaries. Quoting, backspacing etc. doesn't work."

errorExit e = do
  hPutStrLn stderr e
  exitFailure
