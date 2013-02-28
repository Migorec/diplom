{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simulation.HSGPSS.Tests.Blocks.Depart (departTests) where

import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Blocks.Depart
import Test.HUnit
import Control.Monad.State
import Simulation.HSGPSS.Prelude


oneArgTest = TestCase (assertEqual "for (runState (depart \"Name\") initState)," 
                                   (round(0), BlockState [Depart "Name" (round 1)] (round 1)) 
                                   (runState (depart "Name") initState )
                      )

twoArgTest = TestCase (assertEqual "for (runState (depart \"Name\") initState)," 
                                   (round(0), BlockState [Depart "Name" (round 2)] (round 1)) 
                                   (runState (depart ("Name",2)) initState )
                      )
                      
departTests = TestList [TestLabel "oneArgTest" oneArgTest,
                        TestLabel "twoArgTest" twoArgTest
                       ]
