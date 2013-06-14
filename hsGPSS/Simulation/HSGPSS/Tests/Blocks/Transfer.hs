{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simulation.HSGPSS.Tests.Blocks.Transfer (transferTests) where

import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Blocks.Transfer
import Test.HUnit
import Control.Monad.State
import Simulation.HSGPSS.Prelude

unconditionalTest = TestCase (assertEqual "for (runState (transfer ((),1)) initState)," 
                                   (round(0), BlockState [TransferUnconditional (round 1)] (round 1) [])
                                   (runState (transfer ((),1)) initState )
                      )
       
unconditionalIntTest = TestCase (assertEqual "for (runState (transfer ((),1)) initState)," 
                                   (round(0), BlockState [TransferUnconditional (round 1)] (round 1) [])
                                   (runState (transfer ((),round 1 :: Int)) initState )
                      )
       
                      
fractionalOneArgTest = TestCase (assertEqual "for (runState (transfer (0.75,(),1)) initState)," 
                                   (round(0), BlockState [TransferFractional1 0.75 (round 1)] (round 1) []) 
                                   (runState (transfer (0.75,(),1)) initState )
                      )
                      
fractionalOneArgIntTest = TestCase (assertEqual "for (runState (transfer (0.75,(),1)) initState)," 
                                   (round(0), BlockState [TransferFractional1 0.75 (round 1)] (round 1) []) 
                                   (runState (transfer (0.75,(),round 1 :: Int)) initState )
                      )
                      
fractionalTwoArgTest = TestCase (assertEqual "for (runState (transfer (0.75,2,1)) initState)," 
                                   (round(0), BlockState [TransferFractional2 0.75 (round 1) (round 2)] (round 1) []) 
                                   (runState (transfer (0.75,2,1)) initState )
                      )

fractionalTwoArgIntDoubleTest = TestCase (assertEqual "for (runState (transfer (0.75,2,1)) initState)," 
                                   (round(0), BlockState [TransferFractional2 0.75 (round 1) (round 2)] (round 1) []) 
                                   (runState (transfer (0.75,round 2 :: Int,1)) initState )
                      )
                      
fractionalTwoArgDoubleIntTest = TestCase (assertEqual "for (runState (transfer (0.75,2,1)) initState)," 
                                   (round(0), BlockState [TransferFractional2 0.75 (round 1) (round 2)] (round 1) []) 
                                   (runState (transfer (0.75,2,round 1 :: Int)) initState )
                      )
                      
fractionalTwoArgIntIntTest = TestCase (assertEqual "for (runState (transfer (0.75,2,1)) initState)," 
                                   (round(0), BlockState [TransferFractional2 0.75 (round 1) (round 2)] (round 1) []) 
                                   (runState (transfer (0.75,round 2 :: Int,round 1::Int)) initState )
                      )
                      
bothTest = TestCase (assertEqual "for (runState (transfer (Both,1,2)) initState)," 
                                 (round(0), BlockState [TransferBoth (round 1) (round 2)] (round 1) []) 
                                 (runState (transfer (Both,1,2)) initState )
                      )
                      
bothIntDoubleTest = TestCase (assertEqual "for (runState (transfer (Both,1,2)) initState)," 
                                 (round(0), BlockState [TransferBoth (round 1) (round 2)] (round 1) []) 
                                 (runState (transfer (Both,round 1 :: Int,2)) initState )
                      )
                      
bothDoubleIntTest = TestCase (assertEqual "for (runState (transfer (Both,1,2)) initState)," 
                                 (round(0), BlockState [TransferBoth (round 1) (round 2)] (round 1) []) 
                                 (runState (transfer (Both,1,round 2 :: Int)) initState )
                      )
                      
bothIntIntTest = TestCase (assertEqual "for (runState (transfer (Both,1,2)) initState)," 
                                 (round(0), BlockState [TransferBoth (round 1) (round 2)] (round 1) []) 
                                 (runState (transfer (Both,round 1 :: Int,round 2 :: Int)) initState )
                      )
                      
allTwoArgTest = TestCase (assertEqual "for (runState (All,1,10)) initState)," 
                                 (round(0), BlockState [TransferAll (round 1) (round 10) (round 1)] (round 1) []) 
                                 (runState (transfer (All,1,10)) initState )
                         )
          
allTwoArgIntDoubleTest = TestCase (assertEqual "for (runState (All,1,10)) initState)," 
                                 (round(0), BlockState [TransferAll (round 1) (round 10) (round 1)] (round 1) []) 
                                 (runState (transfer (All,round 1 :: Int,10)) initState )
                         )
                         
allTwoArgDoubleIntTest = TestCase (assertEqual "for (runState (All,1,10)) initState)," 
                                 (round(0), BlockState [TransferAll (round 1) (round 10) (round 1)] (round 1) []) 
                                 (runState (transfer (All,1,round 10 :: Int)) initState )
                         )
                         
allTwoArgIntIntTest = TestCase (assertEqual "for (runState (All,1,10)) initState)," 
                                 (round(0), BlockState [TransferAll (round 1) (round 10) (round 1)] (round 1) []) 
                                 (runState (transfer (All,round 1 :: Int,round 10 :: Int)) initState )
                         )
                         
allThreeArgTest = TestCase (assertEqual "for (runState (All,1,10,2)) initState)," 
                                 (round(0), BlockState [TransferAll (round 1) (round 10) (round 2)] (round 1) []) 
                                 (runState (transfer (All,1,10,2)) initState )
                           )     
pickTest = TestCase (assertEqual "for (runState (Pick,1,5)) initState)," 
                                 (round(0), BlockState [TransferPick (round 1) (round 5)] (round 1) []) 
                                 (runState (transfer (Pick,1,5)) initState )
                    ) 
                    
pickIntDoubleTest = TestCase (assertEqual "for (runState (Pick,1,5)) initState)," 
                                 (round(0), BlockState [TransferPick (round 1) (round 5)] (round 1) []) 
                                 (runState (transfer (Pick,round 1 :: Int,5)) initState )
                    ) 
                                
pickDoubleIntTest = TestCase (assertEqual "for (runState (Pick,1,5)) initState)," 
                                 (round(0), BlockState [TransferPick (round 1) (round 5)] (round 1) []) 
                                 (runState (transfer (Pick,1,round 5 :: Int)) initState )
                    ) 
                                
pickIntIntTest = TestCase (assertEqual "for (runState (Pick,1,5)) initState)," 
                                 (round(0), BlockState [TransferPick (round 1) (round 5)] (round 1) []) 
                                 (runState (transfer (Pick,round 1 :: Int,round 5 :: Int)) initState )
                    ) 
                                
parameterTest = TestCase (assertEqual "for (runState (Pick,\"dest\",2)) initState)," 
                                 (round(0), BlockState [TransferParameter "dest" (round 2)] (round 1) []) 
                                 (runState (transfer (P,"dest",2)) initState )
                         )
                          
parameterIntTest = TestCase (assertEqual "for (runState (Pick,\"dest\",2)) initState)," 
                                 (round(0), BlockState [TransferParameter "dest" (round 2)] (round 1) []) 
                                 (runState (transfer (P,"dest",round 2 :: Int)) initState )
                         )
                                 
subroutineTest = TestCase (assertEqual "for (runState (Sbr,5,\"dest\") initState)," 
                                 (round(0), BlockState [TransferSubroutine (round 5) "dest"] (round 1) []) 
                                 (runState (transfer (Sbr,5,"dest")) initState )
                          )
                          
subroutineIntTest = TestCase (assertEqual "for (runState (Sbr,5,\"dest\") initState)," 
                                 (round(0), BlockState [TransferSubroutine (round 5) "dest"] (round 1) []) 
                                 (runState (transfer (Sbr,round 5 :: Int,"dest")) initState )
                          )
                          
multipleBlocks = do first <- transfer ((),1)
                    second <- transfer ((),2)
                    transfer (0.5,first,second)
 
multipleBlocksTest = TestCase (assertEqual "for (runState multipleBlocks initState),"
                                           (round 2, BlockState [TransferFractional2 0.5 (round 1) (round 0),
                                                                 TransferUnconditional (round 2),
                                                                 TransferUnconditional (round 1)
                                                                ] (round 3) []
                                           )
                                           (runState multipleBlocks initState)  
                              )
 
                                 
transferTests = TestList [TestLabel "unconditionalTest" unconditionalTest,
                          TestLabel "unconditionalIntTest" unconditionalIntTest,
                          TestLabel "fractionalOneArgTest" fractionalOneArgTest,
                          TestLabel "fractionalOneArgIntTest" fractionalOneArgIntTest,
                          TestLabel "fractionalTwoArgTest" fractionalTwoArgTest,
                          TestLabel "fractionalTwoArgIntDoubleTest" fractionalTwoArgIntDoubleTest,
                          TestLabel "fractionalTwoArgDoubleIntTest" fractionalTwoArgDoubleIntTest,
                          TestLabel "fractionalTwoArgIntIntTest" fractionalTwoArgIntIntTest,
                          TestLabel "bothTest" bothTest,
                          TestLabel "bothIntDoubleTest" bothIntDoubleTest,
                          TestLabel "bothDoubleIntTest" bothDoubleIntTest,
                          TestLabel "bothIntIntTest" bothIntIntTest,
                          TestLabel "allTwoArgTest" allTwoArgTest,
                          TestLabel "allTwoArgIntDoubleTest" allTwoArgIntDoubleTest,
                          TestLabel "allTwoArgDoubleIntTest" allTwoArgDoubleIntTest,
                          TestLabel "allTwoArgIntIntTest" allTwoArgIntIntTest,
                          TestLabel "allThreeArgTest" allThreeArgTest,
                          TestLabel "pickTest" pickTest,
                          TestLabel "pickIntDoubleTest" pickIntDoubleTest,
                          TestLabel "pickDoubleIntTest" pickDoubleIntTest,
                          TestLabel "pickIntIntTest" pickIntIntTest,
                          TestLabel "parameterTest" parameterTest,
                          TestLabel "parameterIntTest" parameterIntTest,
                          TestLabel "subroutineTest" subroutineTest,
                          TestLabel "subroutineIntTest" subroutineIntTest,
                          TestLabel "multipleBlocksTest" multipleBlocksTest
                         ]
