module Neblen.Main where

import Neblen.Data
import Neblen.Compiler
import System.Console.Haskeline
import System.Process
import Control.Monad.IO.Class

execJS :: JSProgram -> IO String
execJS = readProcess "node" ["-p"]

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "Neblen> "
    case minput of
      Nothing -> outputStrLn "Exiting..."
      Just input -> do
        let js = compile input
        answer <- liftIO (execJS js)
        -- outputStrLn $ js
        outputStrLn answer
        loop
