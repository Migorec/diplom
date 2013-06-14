module Main where

import Simulation.HSGPSS.Tests.Blocks.Generate
import Simulation.HSGPSS.Tests.Blocks.Advance
import Simulation.HSGPSS.Tests.Blocks.Terminate
import Simulation.HSGPSS.Tests.Blocks.Transfer
import Simulation.HSGPSS.Tests.Blocks.Queue
import Simulation.HSGPSS.Tests.Blocks.Depart
import Simulation.HSGPSS.Tests.Blocks.Facility
import Simulation.HSGPSS.Tests.Blocks.Storage
import Simulation.HSGPSS.Tests.Blocks
import Simulation.HSGPSS.Tests.Chains
import Simulation.HSGPSS.Tests.MyMaps
import Simulation.HSGPSS.Tests.SimulationState
import Simulation.HSGPSS.Tests.MyArray
import Simulation.HSGPSS.Tests.Queue
import Simulation.HSGPSS.Tests.Facility
import Simulation.HSGPSS.Tests.Random
import Simulation.HSGPSS.Tests.Storage
import Simulation.HSGPSS.Tests.Simulate.Queue
import Test.HUnit

tests = TestList [ TestLabel "generateTests" generateTests,
                   TestLabel "advanceTests" advanceTests,
                   TestLabel "terminateTests" terminateTests,
                   TestLabel "transferTests" transferTests,
                   TestLabel "queueTests" queueTests,
                   TestLabel "departTests" departTests,
                   TestLabel "facilityTests" facilityTests,
                   TestLabel "storageTests" storageTests,
                   TestLabel "blocksTests" blocksTests,
                   TestLabel "chainsTests" chainsTests,
                   TestLabel "myMapsTests" myMapsTests,
                   TestLabel "simulationStateTests" simulationStateTests,
                   TestLabel "myArrayTest" myArrayTest,
                   TestLabel "sQueueTests" sQueueTests,
                   TestLabel "sFacilityTests" sFacilityTests,
                   TestLabel "randomTests" randomTests,
                   TestLabel "sStorageTests" sStorageTests,
                   TestLabel "squeueTests" squeueTests
                 ]

main = runTestTT tests
