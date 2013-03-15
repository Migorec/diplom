{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}

module Simulation.HSGPSS.Tests.Blocks.Generate (generateTests) where

import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Blocks.Generate
import Test.HUnit
import Control.Monad.State
import Simulation.HSGPSS.Prelude

oneArgTest = TestCase (assertEqual "for (runState (generate 1) initState)," 
                                   (round(0), BlockState [GenerateRangeNoLimit 1 0 0 (round 0)] (round 1)) 
                                   (runState (generate 1) initState )
                      )

twoArgTest = TestCase (assertEqual "for (runState (generate (1, 2)) initState)," 
                                   (round(0), BlockState [GenerateRangeNoLimit 1 2 0 (round 0)] (round 1)) 
                                   (runState (generate (1, 2)) initState)
                      )

threeArgTest = TestCase (assertEqual "for (runState (generate (1, 2, 3)) initState)," 
                                     (round(0), BlockState [GenerateRangeNoLimit 1 2 3 (round 0)] (round 1)) 
                                     (runState (generate (1, 2, 3)) initState)
                        )

fourArgTest = TestCase (assertEqual "for (runState (generate (1, 2, 3, 4)) initState)," 
                                   (round(0), BlockState [GenerateRangeGeneral 1 2 3 (round 4) (round 0)] (round 1)) 
                                   (runState (generate (1, 2, 3, 4)) initState)
                      )

fiveArgNoLimitTest = TestCase (assertEqual "for (runState (generate (1, 2, 3, (), 5)) initState)," 
                                   (round(0), BlockState [GenerateRangeNoLimit 1 2 3 (round 5)] (round 1)) 
                                   (runState (generate (1, 2, 3, (), 5)) initState)
                              )
                              
fiveArgTest = TestCase (assertEqual "for (runState (generate (1, 2, 3, 4, 5)) initState)," 
                                   (round(0), BlockState [GenerateRangeGeneral 1 2 3 (round 4) (round 5)] (round 1)) 
                                   (runState (generate (1, 2, 3, 4, 5)) initState)
                      )

multipleBlocks :: BlockStateMonad
multipleBlocks = do generate 1 
                    generate (1, 2) 
                    generate (1, 2,3) 
                    


multipleBlocksTest = TestCase (assertEqual "for (runState multipleBlocks initState),"
                                           (round(2), BlockState [GenerateRangeNoLimit 1 0 0 (round 0),
                                                                  GenerateRangeNoLimit 1 2 0 (round 0),
                                                                  GenerateRangeNoLimit 1 2 3 (round 0)
                                                                 ] (round 3))
                                           (runState multipleBlocks initState)
                              )

generateTests = TestList [TestLabel "oneArgTest" oneArgTest,
                          TestLabel "twoArgTest" twoArgTest,
                          TestLabel "threeArgTest" threeArgTest,
                          TestLabel "fourArgTest" fourArgTest,
                          TestLabel "fiveArgNoLimitTest" fiveArgNoLimitTest,
                          TestLabel "fiveArgTest" fiveArgTest
                         ]

