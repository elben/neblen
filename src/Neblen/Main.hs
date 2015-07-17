module Neblen.Main where

import Neblen.Compiler
import System.Console.Haskeline

-- TODO call out to node and print result:
--
-- node -e "3+4" -p

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Neblen> "
    case minput of
      Nothing -> outputStrLn "Exiting..."
      Just input -> do
        outputStrLn $ compile input
        loop
