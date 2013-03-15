module Main where

import Simulation.HSGPSS.Tests.Blocks.Generate
import Simulation.HSGPSS.Tests.Blocks.Advance
import Test.HUnit

tests = TestList [ TestLabel "generateTests" generateTests,
                   TestLabel "advanceTests" advanceTests
                 ]

main = runTestTT tests
