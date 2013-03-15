{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simulation.HSGPSS.Tests.Blocks.Queue (queueTests) where

import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Blocks.Queue
import Test.HUnit
import Control.Monad.State
import Simulation.HSGPSS.Prelude


oneArgTest = TestCase (assertEqual "for (runState (queue \"Name\") initState)," 
                                   (round(0), BlockState [Queue "Name" (round 1)] (round 1)) 
                                   (runState (queue "Name") initState )
                      )

twoArgTest = TestCase (assertEqual "for (runState (queue \"Name\") initState)," 
                                   (round(0), BlockState [Queue "Name" (round 2)] (round 1)) 
                                   (runState (queue ("Name",2)) initState )
                      )
                      
queueTests = TestList [TestLabel "oneArgTest" oneArgTest,
                       TestLabel "twoArgTest" twoArgTest
                      ]
