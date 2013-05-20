{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simulation.HSGPSS.Tests.SimulationState (simulationStateTests) where

import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Blocks.Generate
import Simulation.HSGPSS.Blocks.Advance
import Simulation.HSGPSS.Blocks.Terminate
import Simulation.HSGPSS.SimulationState
import Test.HUnit
import Data.Array
import Data.Map
import Simulation.HSGPSS.Prelude


model :: BlockStateMonad
model = do generate (5,2)
           advance (3,1)
           terminate 1


ssInitTest = TestCase (assertEqual "for (ssInit model 10)," 
                                   (SimulationState [] [] (listArray (round 0,round 2) [SBlock (GenerateRangeNoLimit 5 2 0 (round 0)) $ round 0, SBlock (AdvanceRange 3 1) $ round 1, SBlock (Terminate (round 1)) $ round 2]) empty empty (round 10) 0 ) 
                                   (ssInit model $ round 10)
                           )



simulationStateTests = TestList [TestLabel "ssInitTest" ssInitTest]
