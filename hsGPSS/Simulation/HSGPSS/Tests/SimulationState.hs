{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simulation.HSGPSS.Tests.SimulationState (simulationStateTests) where

import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Blocks.Generate
import Simulation.HSGPSS.Blocks.Advance
import Simulation.HSGPSS.Blocks.Terminate
import Simulation.HSGPSS.Blocks.Storage
import Simulation.HSGPSS.SimulationState
import Simulation.HSGPSS.Storage
import Test.HUnit
import Data.Array
import Data.Map
import Simulation.HSGPSS.Prelude


model :: BlockStateMonad
model = do generate (5,2)
           advance (3,1)
           terminate 1

sModel :: BlockStateMonad
sModel = do storage ("SName",2)
            generate (5,2)
            enter "SName"
            leave "SName"
            terminate 1


ssInitTest = TestCase (assertEqual "for (ssInit model 10)," 
                                   (SimulationState [] [] (listArray (round 0,round 2) [SBlock (GenerateRangeNoLimit 5 2 0 (round 0)) $ round 0, SBlock (AdvanceRange 3 1) $ round 1, SBlock (Terminate (round 1)) $ round 2]) empty empty empty (round 10) 0 ) 
                                   (ssInit model $ round 10)
                           )
                           
ssInitWithStorTest = TestCase (assertEqual "for (ssInit sModel 10)," 
                                   (SimulationState [] [] (listArray (round 0,round 3) [SBlock (GenerateRangeNoLimit 5 2 0 (round 0)) $ round 0, SBlock (Enter "SName" $ round 1) $ round 1, SBlock (Leave "SName" $ round 1) $ round 2, SBlock (Terminate (round 1)) $ round 3]) empty empty (singleton "SName" (stInit $ round 2)) (round 10) 0 ) 
                                   (ssInit sModel $ round 10)
                           )



simulationStateTests = TestList [TestLabel "ssInitTest" ssInitTest,
                                 TestLabel "ssInitWithStorTest" ssInitWithStorTest]
