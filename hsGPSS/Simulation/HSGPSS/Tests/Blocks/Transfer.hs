{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simulation.HSGPSS.Tests.Blocks.Transfer (transferTests) where

import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Blocks.Transfer
import Test.HUnit
import Control.Monad.State
import Simulation.HSGPSS.Prelude

unconditionalTest = TestCase (assertEqual "for (runState (transfer ((),1)) initState)," 
                                   (round(0), BlockState [TransferUnconditional (round 1)] (round 1)) 
                                   (runState (transfer ((),1)) initState )
                      )
                      
fractionalOneArgTest = TestCase (assertEqual "for (runState (transfer (0.75,(),1)) initState)," 
                                   (round(0), BlockState [TransferFractional1 0.75 (round 1)] (round 1)) 
                                   (runState (transfer (0.75,(),1)) initState )
                      )
                      
fractionalTwoArgTest = TestCase (assertEqual "for (runState (transfer (0.75,2,1)) initState)," 
                                   (round(0), BlockState [TransferFractional2 0.75 (round 1) (round 2)] (round 1)) 
                                   (runState (transfer (0.75,2,1)) initState )
                      )
                      
bothTest = TestCase (assertEqual "for (runState (transfer (Both,1,2)) initState)," 
                                 (round(0), BlockState [TransferBoth (round 1) (round 2)] (round 1)) 
                                 (runState (transfer (Both,1,2)) initState )
                      )
                      
allTwoArgTest = TestCase (assertEqual "for (runState (All,1,10)) initState)," 
                                 (round(0), BlockState [TransferAll (round 1) (round 10) (round 1)] (round 1)) 
                                 (runState (transfer (All,1,10)) initState )
                         )
                         
allThreeArgTest = TestCase (assertEqual "for (runState (All,1,10,2)) initState)," 
                                 (round(0), BlockState [TransferAll (round 1) (round 10) (round 2)] (round 1)) 
                                 (runState (transfer (All,1,10,2)) initState )
                           )     
pickTest = TestCase (assertEqual "for (runState (Pick,1,5)) initState)," 
                                 (round(0), BlockState [TransferPick (round 1) (round 5)] (round 1)) 
                                 (runState (transfer (Pick,1,5)) initState )
                    )             
parameterTest = TestCase (assertEqual "for (runState (Pick,\"dest\",2)) initState)," 
                                 (round(0), BlockState [TransferParameter "dest" (round 2)] (round 1)) 
                                 (runState (transfer (P,"dest",2)) initState )
                         )
                                 
subroutineTest = TestCase (assertEqual "for (runState (Sbr,5,\"dest\") initState)," 
                                 (round(0), BlockState [TransferSubroutine (round 5) "dest"] (round 1)) 
                                 (runState (transfer (Sbr,5,"dest")) initState )
                          )
                          
multipleBlocks = do first <- transfer ((),1)
                    second <- transfer ((),2)
                    transfer (0.5,first,second)
 
multipleBlocksTest = TestCase (assertEqual "for (runState multipleBlocks initState),"
                                           (round 2, BlockState [TransferFractional2 0.5 (round 1) (round 0),
                                                                 TransferUnconditional (round 2),
                                                                 TransferUnconditional (round 1)
                                                                ] (round 3)
                                           )
                                           (runState multipleBlocks initState)  
                              )
 
                                 
transferTests = TestList [TestLabel "unconditionalTest" unconditionalTest,
                          TestLabel "fractionalOneArgTest" fractionalOneArgTest,
                          TestLabel "fractionalTwoArgTest" fractionalTwoArgTest,
                          TestLabel "bothTest" bothTest,
                          TestLabel "allTwoArgTest" allTwoArgTest,
                          TestLabel "allThreeArgTest" allThreeArgTest,
                          TestLabel "pickTest" pickTest,
                          TestLabel "parameterTest" parameterTest,
                          TestLabel "subroutineTest" subroutineTest,
                          TestLabel "multipleBlocksTest" multipleBlocksTest
                         ]
