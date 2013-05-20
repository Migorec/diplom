module Simulation.HSGPSS.Tests.MyArray (myArrayTest) where

import Test.HUnit
import Simulation.HSGPSS.MyArray
import Data.Array

testArray = array (2,4) [(2,1),(3,2),(4,3)]

firstElemTest = TestCase (assertEqual "for (assocs $ changeElem testArray 2 5)," 
                                   ([(2,5),(3,2),(4,3)]) 
                                   (assocs $ changeElem testArray 2 5)
                           )
                           
lastElemTest = TestCase (assertEqual "for (assocs $ changeElem testArray 4 5)," 
                                   ([(2,1),(3,2),(4,5)]) 
                                   (assocs $ changeElem testArray 4 5)
                           )
                           
middleElemTest = TestCase (assertEqual "for (assocs $ changeElem testArray 3 5)," 
                                   ([(2,1),(3,5),(4,3)]) 
                                   (assocs $ changeElem testArray 3 5)
                           )
                           
myArrayTest = TestList [TestLabel "firstElemTest" firstElemTest,
                        TestLabel "middleElemTest" middleElemTest,
                        TestLabel "lastElemTest" lastElemTest
                       ]
