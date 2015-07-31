module Main where

import Test.Framework
import Data.Monoid

import qualified Neblen.TypeChecker.Tests

-- Get empty options and update with our options.
runnerOpts :: RunnerOptions
runnerOpts = mempty {
  ropt_color_mode = Just ColorAlways,
  ropt_hide_successes = Just False
}

main :: IO ()
main =
  defaultMainWithOpts
    [ Neblen.TypeChecker.Tests.tests
    ]
    runnerOpts
