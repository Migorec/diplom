module Main where

import Simulation.HSGPSS.Tests.Blocks.Generate
import Simulation.HSGPSS.Tests.Blocks.Advance
import Simulation.HSGPSS.Tests.Blocks.Terminate
import Simulation.HSGPSS.Tests.Blocks.Transfer
import Simulation.HSGPSS.Tests.Blocks.Queue
import Simulation.HSGPSS.Tests.Blocks.Depart
import Simulation.HSGPSS.Tests.Blocks.Facility
import Test.HUnit

tests = TestList [ TestLabel "generateTests" generateTests,
                   TestLabel "advanceTests" advanceTests,
                   TestLabel "terminateTests" terminateTests,
                   TestLabel "transferTests" transferTests,
                   TestLabel "queueTests" queueTests,
                   TestLabel "departTests" departTests,
                   TestLabel "facilityTests" facilityTests
                 ]

main = runTestTT tests
