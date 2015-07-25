module Main where

import Test.Framework (defaultMain)

import qualified Neblen.TypeChecker.Tests

main :: IO ()
main = defaultMain
  [ Neblen.TypeChecker.Tests.tests
  ]

