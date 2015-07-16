module Neblen.Main where

import Neblen.Compiler
import System.Console.Haskeline

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
