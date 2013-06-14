{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simulation.HSGPSS.Tests.Blocks.Terminate (terminateTests) where

import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Blocks.Terminate
import Test.HUnit
import Control.Monad.State
import Simulation.HSGPSS.Prelude

oneArgTest = TestCase (assertEqual "for (runState (terminate 1) initState)," 
                                   (round(0), BlockState [Terminate (round 1)] (round 1) []) 
                                   (runState (terminate 1) initState )
                      )
                  
oneArgIntTest = TestCase (assertEqual "for (runState (terminate $ round 1 :: Int) initState)," 
                                   (round(0), BlockState [Terminate (round 1)] (round 1) []) 
                                   (runState (terminate (round 1 :: Int)) initState )
                      )
                      
terminateTests = TestList [TestLabel "oneArgTest" oneArgTest,
                           TestLabel "oneArgIntTest" oneArgIntTest
                          ]

