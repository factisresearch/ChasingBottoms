module Main where

import IO
import List
import System.Environment
import System.Posix.Process

ghc = "/users/cs/group_multi/pub/bin/ghc"
stripFirstLine = "/users/cs/nad/bin/haskell-script-helper"

main = do
  args <- getArgs
  if length args < 1 then
    hPutStr stderr usage
   else do
    let file:progArgs = args
        cmd = "System.Environment.withProgName \"" ++ file ++
              "\" $ System.Environment.withArgs [\"" ++
              concat (intersperse "\", \"" progArgs) ++
              "\"] Main.main"
        ghcArgs = ["-F", "-pgmF", stripFirstLine, "-e", cmd, file]
    executeFile ghc False ghcArgs Nothing

usage =
  "Usage: runghc <Haskell source file> [<Extra arguments to the program>...]\n"
