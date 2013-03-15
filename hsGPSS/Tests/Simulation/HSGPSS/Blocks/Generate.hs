{-# LANGUAGE RebindableSyntax #-}

module Tests.Simulation.HSGPSS.Blocks.Generate where

import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Blocks.Generate
import Test.HUnit
import Control.Monad.State

import Prelude hiding (fromInteger)

fromInteger :: Integer -> Double
fromInteger = realToFrac

oneArgTest = TestCase (assertEqual "for (runState (generate 1) initState)," 
                                   (round(0), BlockState [GenerateRangeNoLimit 1 0 0 (round 0)] (round 1)) 
                                   (runState (generate 1 :: BlockStateMonad) initState)
                      )

twoArgTest = TestCase (assertEqual "for (runState (generate 1 2) initState)," 
                                   (round(0), BlockState [GenerateRangeNoLimit 1 2 0 (round 0)] (round 1)) 
                                   (runState (generate 1 2 :: BlockStateMonad) initState)
                      )

threeArgTest = TestCase (assertEqual "for (runState (generate 1 2 3) initState)," 
                                     (round(0), BlockState [GenerateRangeNoLimit 1 2 3 (round 0)] (round 1)) 
                                     (runState (generate 1 2 3 :: BlockStateMonad) initState)
                        )

fourArgTest = TestCase (assertEqual "for (runState (generate 1 2 3 4) initState)," 
                                   (round(0), BlockState [GenerateRangeGeneral 1 2 3 (round 4) (round 0)] (round 1)) 
                                   (runState (generate 1 2 3 4:: BlockStateMonad) initState)
                      )

fiveArgNoLimitTest = TestCase (assertEqual "for (runState (generate 1 2 3 () 5) initState)," 
                                   (round(0), BlockState [GenerateRangeNoLimit 1 2 3 (round 5)] (round 1)) 
                                   (runState (generate 1 2 3 () 5 :: BlockStateMonad) initState)
                              )
                              
fiveArgTest = TestCase (assertEqual "for (runState (generate 1 2 3 4 5) initState)," 
                                   (round(0), BlockState [GenerateRangeGeneral 1 2 3 (round 4) (round 5)] (round 1)) 
                                   (runState (generate 1 2 3 4 5 :: BlockStateMonad) initState)
                      )

multipleBlocks :: BlockStateMonad
multipleBlocks = do generate 1 :: BlockStateMonad
                    generate 1 2 :: BlockStateMonad
                    generate 1 2 3 

multipleBlocksTest = TestCase (assertEqual "for (runState multipleBlocks initState),"
                                           (round(2), BlockState [GenerateRangeNoLimit 1 0 0 (round 0),
                                                                  GenerateRangeNoLimit 1 2 0 (round 0),
                                                                  GenerateRangeNoLimit 1 2 3 (round 0)
                                                                 ] (round 3))
                                           (runState multipleBlocks initState)
                              )

tests = TestList [TestLabel "oneArgTest" oneArgTest,
                  TestLabel "twoArgTest" twoArgTest,
                  TestLabel "threeArgTest" threeArgTest,
                  TestLabel "fourArgTest" fourArgTest,
                  TestLabel "fiveArgNoLimitTest" fiveArgNoLimitTest,
                  TestLabel "fiveArgTest" fiveArgTest
                 ]

