module Simulation.HSGPSS.Tests.Chains (chainsTests) where


import Data.Array
import Test.HUnit
import Data.IntMap
import Simulation.HSGPSS.Chains
import Simulation.HSGPSS.Transaction

defTransact = Transaction 0 0 0 Active empty
specTransact = Transaction 1 0 0 Active empty

emptyFEC = TestCase (assertEqual "for (addFE [] (1,defTransact))," 
                                   ([(1,defTransact)]) 
                                   (addFE [] (1,defTransact))
                           )

middleAddFE = TestCase (assertEqual "for (addFE [(1,defTransact),(3,defTransact)] (2,defTransact))," 
                                   ([(1,defTransact),(2,defTransact),(3,defTransact)]) 
                                   (addFE [(1,defTransact),(3,defTransact)] (2,defTransact))
                           )
                           
nearestAddFE = TestCase (assertEqual "for (addFE [(2,defTransact),(3,defTransact)] (1,defTransact))," 
                                   ([(1,defTransact),(2,defTransact),(3,defTransact)]) 
                                   (addFE [(2,defTransact),(3,defTransact)] (1,defTransact))
                           )
                           
lastAddFE = TestCase (assertEqual "for (addFE [(1,defTransact),(2,defTransact)] (3,defTransact))," 
                                   ([(1,defTransact),(2,defTransact),(3,defTransact)]) 
                                   (addFE [(1,defTransact),(2,defTransact)] (3,defTransact))
                           )
                           
multyAddFE = TestCase (assertEqual "for (addFE [(1,defTransact),(2,defTransact),(2,defTransact), (3,defTransact)] (3,specTransact))," 
                                   ([(1,defTransact),(2,defTransact),(2,defTransact),(2,specTransact),(3,defTransact)]) 
                                   (addFE [(1,defTransact),(2,defTransact),(2,defTransact),(3,defTransact)] (2,specTransact))
                           )
                           
chainsTests = TestList [TestLabel "emptyFEC" emptyFEC,
                        TestLabel "middleAddFE" middleAddFE,
                        TestLabel "nearestAddFE" nearestAddFE,
                        TestLabel "lastAddFE" lastAddFE,
                        TestLabel "multyAddFE" multyAddFE
                       ]
