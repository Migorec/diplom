{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simulation.HSGPSS.Tests.Blocks.Facility (facilityTests) where

import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Blocks.Facility
import Test.HUnit
import Control.Monad.State
import Simulation.HSGPSS.Prelude


seizeTest = TestCase (assertEqual "for (runState (seize \"Name\") initState)," 
                                   (round(0), BlockState [Seize "Name"] (round 1) []) 
                                   (runState (seize "Name") initState )
                      )
                      
releaseTest = TestCase (assertEqual "for (runState (release \"Name\") initState)," 
                                   (round(0), BlockState [Release "Name"] (round 1) []) 
                                   (runState (release "Name") initState )
                      )
                      
returnTest = TestCase (assertEqual "for (runState (return' \"Name\") initState)," 
                                   (round(0), BlockState [Return "Name"] (round 1) []) 
                                   (runState (return' "Name") initState )
                      )

favailTest = TestCase (assertEqual "for (runState (faval \"Name\") initState),"
                                  (round 0, BlockState [FAvail "Name"] (round 1) [])
                                  (runState (favail "Name") initState)
                     )
                    
preemptOneArgTest = TestCase (assertEqual "for (runState (preempt \"Name\") initState)," 
                                   (round(0), BlockState [PreemptIR "Name" Nothing Nothing False] (round 1) []) 
                                   (runState (preempt "Name") initState )
                      )
                      
preemptTwoArgTest = TestCase (assertEqual "for (runState (preempt (\"Name\", PR)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" Nothing Nothing False] (round 1) []) 
                                   (runState (preempt ("Name", PR)) initState )
                      )
                      
preemptIRDestTest = TestCase (assertEqual "for (runState (preempt (\"Name\",(),5)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) Nothing False] (round 1) []) 
                                   (runState (preempt ("Name", (), 5)) initState )
                      )
                      
preemptPRDestTest = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,5)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) Nothing False] (round 1) []) 
                                   (runState (preempt ("Name", PR, 5)) initState )
                             )
                                    
preemptIRParTest = TestCase (assertEqual "for (runState (preempt (\"Name\",(),(),3)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" Nothing (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", (), (),3)) initState )
                            )
                                  
preemptPRParTest = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,(),3)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" Nothing (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", PR, (),3)) initState )
                            )
                                  
preemptPRDestParTest = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,5,3)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", PR, 5,3)) initState )
                                )
                                   
preemptIRDestParTest = TestCase (assertEqual "for (runState (preempt (\"Name\",(),5,3)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", (), 5,3)) initState )
                                )  
                                   
preemptIRRem = TestCase (assertEqual "for (runState (preempt (\"Name\",(),5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) Nothing True] (round 1) []) 
                                   (runState (preempt ("Name", (), 5,(),RE)) initState )
                        )
                                   
preemptPRRem = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) Nothing True] (round 1) []) 
                                   (runState (preempt ("Name", PR, 5,(),RE)) initState )
                        )
                                   
preemptIRRemPar = TestCase (assertEqual "for (runState (preempt (\"Name\",(),5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) (Just $ round 3) True] (round 1) []) 
                                   (runState (preempt ("Name", (), 5,3,RE)) initState )
                           )
                                  
preemptPRRemPar = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) (Just $ round 3) True] (round 1) []) 
                                   (runState (preempt ("Name", PR, 5,3,RE)) initState )
                           )        
                                   
facilityTests = TestList [ TestLabel "seizeTest" seizeTest,
                           TestLabel "releaseTest" releaseTest,
                           TestLabel "returnTest" returnTest,
                           TestLabel "favailTest" favailTest,
                           TestLabel "preemptOneArgTest" preemptOneArgTest,
                           TestLabel "preemptTwoArgTest" preemptTwoArgTest,
                           TestLabel "preemptIRDestTest" preemptIRDestTest,
                           TestLabel "preemptPRDestTest" preemptPRDestTest,
                           TestLabel "preemptIRParTest" preemptIRParTest,
                           TestLabel "preemptPRParTest" preemptPRParTest,
                           TestLabel "preemptIRDestParTest" preemptIRDestParTest,
                           TestLabel "preemptPRDestParTest" preemptPRDestParTest,
                           TestLabel "preemptIRRem" preemptIRRem,
                           TestLabel "preemptPRRem" preemptPRRem,
                           TestLabel "preemptIRRemPar" preemptIRRemPar,
                           TestLabel "preemptPRRemPar" preemptPRRemPar
                          ] 
