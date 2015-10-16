module Main where

import Neblen.Data
import Neblen.Eval
import Neblen.Utils
import System.Console.Haskeline
import System.Process
import Data.List

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
        let typeCheck = ":t " `isPrefixOf` input
        let input' = if typeCheck then input \\ ":t " else input
        -- TODO only checkType if :t?
        let answer = parseAndEval input'
        case answer of
          Left e -> outputStrLn e
          Right (e, t) ->
            if typeCheck
            then outputStrLn (input' ++ " : " ++ show t)
            else outputStrLn (toLisp e ++ " : " ++ show t)
        loop
