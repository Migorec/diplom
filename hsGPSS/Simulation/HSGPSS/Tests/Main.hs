module Main where

import Simulation.HSGPSS.Tests.Blocks.Generate
import Simulation.HSGPSS.Tests.Blocks.Advance
import Simulation.HSGPSS.Tests.Blocks.Terminate
import Test.HUnit

tests = TestList [ TestLabel "generateTests" generateTests,
                   TestLabel "advanceTests" advanceTests,
                   TestLabel "terminateTests" terminateTests
                 ]

main = runTestTT tests
