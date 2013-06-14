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
                      
preemptIRDestIntTest = TestCase (assertEqual "for (runState (preempt (\"Name\",(),round 5 :: Int)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) Nothing False] (round 1) []) 
                                   (runState (preempt ("Name", (), round 5::Int)) initState )
                      )
                      
preemptPRDestTest = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,5)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) Nothing False] (round 1) []) 
                                   (runState (preempt ("Name", PR, 5)) initState )
                             )
                             
preemptPRDestIntTest = TestCase (assertEqual "for (runState (preempt (\"Name\",PR, round 5 :: Int)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) Nothing False] (round 1) []) 
                                   (runState (preempt ("Name", PR, round 5 :: Int)) initState )
                             )
                                    
preemptIRParTest = TestCase (assertEqual "for (runState (preempt (\"Name\",(),(),3)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" Nothing (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", (), (),3)) initState )
                            )
                            
preemptIRParIntTest = TestCase (assertEqual "for (runState (preempt (\"Name\",(),(),round 3 :: Int)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" Nothing (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", (), (),round 3 :: Int)) initState )
                            )
                                  
preemptPRParTest = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,(),3)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" Nothing (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", PR, (),3)) initState )
                            )
                            
preemptPRParIntTest = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,(),round 3 :: Int)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" Nothing (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", PR, (),round 3 :: Int)) initState )
                            )
                                  
preemptPRDestParTest = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,5,3)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", PR, 5,3)) initState )
                                )
                                
preemptPRDestParIntDoubleTest = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,round 5 :: Int,3)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", PR, round 5 :: Int,3)) initState )
                                )
   
preemptPRDestParDoubleIntTest = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,5,round 3 :: Int)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", PR, 5,round 3 :: Int)) initState )
                                )
                                   
preemptPRDestParIntIntTest = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,5,3)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", PR,round  5 :: Int, round 3 :: Int)) initState )
                                )
                                   
preemptIRDestParTest = TestCase (assertEqual "for (runState (preempt (\"Name\",(),5,3)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", (), 5,3)) initState )
                                )  
                                
preemptIRDestParIntDoubleTest = TestCase (assertEqual "for (runState (preempt (\"Name\",(),5,3)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", (), round 5 :: Int,3)) initState )
                                )  
                                  
preemptIRDestParDoubleIntTest = TestCase (assertEqual "for (runState (preempt (\"Name\",(),5,3)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", (), 5,round 3 :: Int)) initState )
                                )  
                                
preemptIRDestParIntIntTest = TestCase (assertEqual "for (runState (preempt (\"Name\",(),5,3)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) (Just $ round 3) False] (round 1) []) 
                                   (runState (preempt ("Name", (), round 5 :: Int,round 3 :: Int)) initState )
                                )  
                                  
                                  
                                   
preemptIRRem = TestCase (assertEqual "for (runState (preempt (\"Name\",(),5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) Nothing True] (round 1) []) 
                                   (runState (preempt ("Name", (), 5,(),RE)) initState )
                        )
                        
preemptIRRemInt = TestCase (assertEqual "for (runState (preempt (\"Name\",(),5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) Nothing True] (round 1) []) 
                                   (runState (preempt ("Name", (), round 5 :: Int,(),RE)) initState )
                        )
                                   
preemptPRRem = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) Nothing True] (round 1) []) 
                                   (runState (preempt ("Name", PR, 5,(),RE)) initState )
                        )
                         
preemptPRRemInt = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) Nothing True] (round 1) []) 
                                   (runState (preempt ("Name", PR, round 5 :: Int,(),RE)) initState )
                        )
                                   
preemptIRRemPar = TestCase (assertEqual "for (runState (preempt (\"Name\",(),5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) (Just $ round 3) True] (round 1) []) 
                                   (runState (preempt ("Name", (), 5,3,RE)) initState )
                           )
                           
preemptIRRemParIntDouble = TestCase (assertEqual "for (runState (preempt (\"Name\",(),5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) (Just $ round 3) True] (round 1) []) 
                                   (runState (preempt ("Name", (), round 5 :: Int,3,RE)) initState )
                           )
                                  
preemptIRRemParDoubleInt = TestCase (assertEqual "for (runState (preempt (\"Name\",(),5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) (Just $ round 3) True] (round 1) []) 
                                   (runState (preempt ("Name", (), 5,round 3 :: Int,RE)) initState )
                           )
                           
preemptIRRemParIntInt = TestCase (assertEqual "for (runState (preempt (\"Name\",(),5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptIR "Name" (Just $ round 5) (Just $ round 3) True] (round 1) []) 
                                   (runState (preempt ("Name", (), round 5 :: Int,round 3 :: Int,RE)) initState )
                           )
                                  
preemptPRRemPar = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) (Just $ round 3) True] (round 1) []) 
                                   (runState (preempt ("Name", PR, 5,3,RE)) initState )
                           )        
                             
preemptPRRemParIntDouble = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) (Just $ round 3) True] (round 1) []) 
                                   (runState (preempt ("Name", PR, round 5 :: Int,3,RE)) initState )
                           ) 
                           
preemptPRRemParDoubleInt = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) (Just $ round 3) True] (round 1) []) 
                                   (runState (preempt ("Name", PR, 5,round 3 :: Int,RE)) initState )
                           ) 
                           
preemptPRRemParIntInt = TestCase (assertEqual "for (runState (preempt (\"Name\",PR,5,(),RE)) initState)," 
                                   (round(0), BlockState [PreemptPR "Name" (Just $ round 5) (Just $ round 3) True] (round 1) []) 
                                   (runState (preempt ("Name", PR, round 5 :: Int, round 3 :: Int,RE)) initState )
                           ) 
                                   
facilityTests = TestList [ TestLabel "seizeTest" seizeTest,
                           TestLabel "releaseTest" releaseTest,
                           TestLabel "returnTest" returnTest,
                           TestLabel "favailTest" favailTest,
                           TestLabel "preemptOneArgTest" preemptOneArgTest,
                           TestLabel "preemptTwoArgTest" preemptTwoArgTest,
                           TestLabel "preemptIRDestTest" preemptIRDestTest,
                           TestLabel "preemptIRDestIntTest" preemptIRDestIntTest,
                           TestLabel "preemptPRDestTest" preemptPRDestTest,
                           TestLabel "preemptPRDestIntTest" preemptPRDestIntTest,
                           TestLabel "preemptIRParTest" preemptIRParTest,
                           TestLabel "preemptIRParIntTest" preemptIRParIntTest,
                           TestLabel "preemptPRParTest" preemptPRParTest,
                           TestLabel "preemptPRParIntTest" preemptPRParIntTest,
                           TestLabel "preemptIRDestParTest" preemptIRDestParTest,
                           TestLabel "preemptIRDestParIntDoubleTest" preemptIRDestParIntDoubleTest,
                           TestLabel "preemptIRDestParDoubleIntTest" preemptIRDestParDoubleIntTest,
                           TestLabel "preemptIRDestParIntIntTest" preemptIRDestParIntIntTest,
                           TestLabel "preemptPRDestParTest" preemptPRDestParTest,
                           TestLabel "preemptPRDestParIntDoubleTest" preemptPRDestParIntDoubleTest,
                           TestLabel "preemptPRDestParDoubleIntTest" preemptPRDestParDoubleIntTest,
                           TestLabel "preemptPRDestParIntIntTest" preemptPRDestParIntIntTest,
                           TestLabel "preemptIRRem" preemptIRRem,
                           TestLabel "preemptIRRemInt" preemptIRRemInt,
                           TestLabel "preemptPRRem" preemptPRRem,
                           TestLabel "preemptPRRemInt" preemptPRRemInt,
                           TestLabel "preemptIRRemPar" preemptIRRemPar,
                           TestLabel "preemptIRRemParIntDouble" preemptIRRemParIntDouble,
                           TestLabel "preemptIRRemParDoubleInt" preemptIRRemParDoubleInt,
                           TestLabel "preemptIRRemParIntInt" preemptIRRemParIntInt,
                           TestLabel "preemptPRRemPar" preemptPRRemPar,
                           TestLabel "preemptPRRemParIntDouble" preemptPRRemParIntDouble,
                           TestLabel "preemptPRRemParDoubleInt" preemptPRRemParDoubleInt,
                           TestLabel "preemptPRRemParIntInt" preemptPRRemParIntInt
                          ] 
