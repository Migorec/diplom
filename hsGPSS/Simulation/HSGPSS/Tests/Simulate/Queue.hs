
module Simulation.HSGPSS.Tests.Simulate.Queue (squeueTests) where

import Simulation.HSGPSS.Simulate.Queue
import Simulation.HSGPSS.Queue
import Simulation.HSGPSS.SimulationState
import Simulation.HSGPSS.Transaction
import Simulation.HSGPSS.Blocks
import qualified Data.IntMap as IM
import Data.Map
import Data.Array
import Test.HUnit


defTransact :: Transaction
defTransact = Transaction 0 0 0 Active IM.empty ""

ss = SimulationState [] [] (listArray (0,0) [SBlock (Queue "qn" 1) 0]) empty empty empty 1 1

queueTest = TestCase ( assertEqual "for (queue'  ss (SBlock (Queue \"qn\" 1) 0) defTransact)"
                                   (SimulationState [] [defTransact{currentBlock = 0,nextBlock = 1}]
                                                    (listArray (0,0) [SBlock (Queue "qn" 1) 0]) empty (singleton "qn" (SQueue 1 1 0 1)) empty 1 1)
                                   (queue'  ss (SBlock (Queue "qn" 1) 0) defTransact)

                     )

ss' = SimulationState [] [] (listArray (0,0) [SBlock (Depart "qn" 1) 0]) empty empty empty 1 1

departTest = TestCase ( assertEqual "for (depart'  ss{queues = singleton \"qn\" (SQueue 1 1 0 1)} (SBlock (Depart \"qn\" 1) 0) defTransact)"
                                   (SimulationState [] [defTransact{currentBlock = 0,nextBlock = 1}]
                                                    (listArray (0,0) [SBlock (Depart "qn" 1) 0]) empty (singleton "qn" (SQueue 0 1 0 1)) empty 1 1)
                                   (depart'  ss'{queues = singleton "qn" (SQueue 1 1 0 1)} (SBlock (Depart "qn" 1) 0) defTransact)

                     )


squeueTests = TestList [TestLabel "queueTest" queueTest,
                        TestLabel "departTest" departTest]
