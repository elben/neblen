module Neblen.Parser.Tests where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import qualified Data.Map.Strict as M

import Neblen.Parser
import Neblen.Data

tests :: Test
tests = testGroup "Neblen.Parser.Tests" $ concat
    [
    ]

