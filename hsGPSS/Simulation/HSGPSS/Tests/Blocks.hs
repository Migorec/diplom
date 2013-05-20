{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simulation.HSGPSS.Tests.Blocks (blocksTests) where

import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Blocks.Generate
import Simulation.HSGPSS.Blocks.Advance
import Simulation.HSGPSS.Blocks.Terminate
import Data.Array
import Test.HUnit
import Control.Monad.State
import Simulation.HSGPSS.Prelude


model :: BlockStateMonad
model = do generate (5,2)
           advance (3,1)
           terminate 1


createModelTest = TestCase (assertEqual "for (createModel model)," 
                                   (listArray (round(0),round(2)) [GenerateRangeNoLimit 5 2 0 (round 0),
                                                     AdvanceRange 3 1,
                                                     Terminate (round 1)]) 
                                   (createModel model)
                           )
                      
blocksTests = TestList [TestLabel "createModelTest" createModelTest]
