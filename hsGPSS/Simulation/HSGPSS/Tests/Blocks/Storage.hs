{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simulation.HSGPSS.Tests.Blocks.Storage (storageTests) where

import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Blocks.Storage
import Test.HUnit
import Control.Monad.State
import Simulation.HSGPSS.Prelude


storageTest = TestCase (assertEqual "for (runState (storage (\"Name\", 5)) initState),"
                                    ((), BlockState [] (round 0) [Storage "Name" (round 5)])
                                    (runState (storage ("Name",5)) initState)
                       )
                       
enterTest = TestCase (assertEqual "for (runState (enter (\"Name\", 5)) initState),"
                                    ((round 0), BlockState [Enter "Name" (round 5)] (round 1) [])
                                    (runState (enter ("Name",5)) initState)
                       )
                        
enterDefaultTest = TestCase (assertEqual "for (runState (enter \"Name\") initState),"
                                    ((round 0), BlockState [Enter "Name" (round 1)] (round 1) [])
                                    (runState (enter "Name") initState)
                       )
                        
leaveTest = TestCase (assertEqual "for (runState (leave (\"Name\", 5)) initState),"
                                    ((round 0), BlockState [Leave "Name" (round 5)] (round 1) [])
                                    (runState (leave ("Name",5)) initState)
                       )
                        
leaveDefaultTest = TestCase (assertEqual "for (runState (leave \"Name\") initState),"
                                    ((round 0), BlockState [Leave "Name" (round 1)] (round 1) [])
                                    (runState (leave "Name") initState)
                       )
                                    
storageTests = TestList [TestLabel "storageTest" storageTest,
                         TestLabel "enterTest" enterTest,
                         TestLabel "enterDefaultTest" enterDefaultTest,
                         TestLabel "leaveTest" leaveTest,
                         TestLabel "leaveDefaultTest" leaveDefaultTest]
