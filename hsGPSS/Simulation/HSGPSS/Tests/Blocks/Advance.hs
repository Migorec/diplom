{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simulation.HSGPSS.Tests.Blocks.Advance (advanceTests) where

import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Blocks.Advance
import Test.HUnit
import Control.Monad.State
import Simulation.HSGPSS.Prelude

oneArgTest = TestCase (assertEqual "for (runState (advance 1) initState)," 
                                   (round(0), BlockState [AdvanceRange 1 0] (round 1)) 
                                   (runState (advance 1) initState )
                      )

twoArgTest = TestCase (assertEqual "for (runState (advance 1 2) initState)," 
                                   (round(0), BlockState [AdvanceRange 1 2] (round 1)) 
                                   (runState (advance (1,2)) initState )
                      )
                      
advanceTests = TestList [ TestLabel "oneArgTest" oneArgTest,
                          TestLabel "twoArgTest" twoArgTest
                        ]
