module Simulation.HSGPSS.Tests.Chains (chainsTests) where


import Data.Array
import Test.HUnit
import Data.IntMap
import Simulation.HSGPSS.Chains
import Simulation.HSGPSS.Transaction

defTransact :: Transaction
defTransact = Transaction 0 0 0 Active empty ""

specTransact :: Transaction
specTransact = Transaction 1 0 0 Active empty ""

ownerTransact :: Transaction
ownerTransact = Transaction 1 0 0 Passive empty "foo"

lowPTransact :: Transaction
lowPTransact = Transaction 0 1 0 Active empty ""

midPTransact :: Transaction
midPTransact = Transaction 0 1 1 Active empty ""

highPTransact :: Transaction
highPTransact = Transaction 0 1 2 Active empty ""

emptyFEC = TestCase (assertEqual "for (addFE [] (1,defTransact))," 
                                   ([(1,defTransact)]) 
                                   (addFE [] (1,defTransact))
                           )

emptyPC = TestCase ( assertEqual "for (addPC [] lowPTransact),"
                                  ([lowPTransact])
                                  (addPC [] lowPTransact)

                   )

middleAddFE = TestCase (assertEqual "for (addFE [(1,defTransact),(3,defTransact)] (2,defTransact))," 
                                   ([(1,defTransact),(2,defTransact),(3,defTransact)]) 
                                   (addFE [(1,defTransact),(3,defTransact)] (2,defTransact))
                           )
                           
middleAddPC = TestCase (assertEqual "for (addPC [highPTransact,lowPTransact] midPTransact)," 
                                   ([highPTransact,midPTransact,lowPTransact]) 
                                   (addPC [highPTransact,lowPTransact] midPTransact)
                           )
                           
nearestAddPC = TestCase (assertEqual "for (addPC [midPTransact,lowPTransact] highPTransact)," 
                                   ([highPTransact,midPTransact,lowPTransact]) 
                                   (addPC [midPTransact,lowPTransact] highPTransact)
                           )
                           
                           
multyAddPC = TestCase (assertEqual "for (addPC [highPTransact, midPTransact,lowPTransact] midPTransact{nextBlock = 10})," 
                                   ([highPTransact,midPTransact,midPTransact{nextBlock = 10}, lowPTransact]) 
                                   (addPC [highPTransact, midPTransact,lowPTransact] midPTransact{nextBlock = 10})
                           )
                           
lastAddPC = TestCase (assertEqual "for (addPC [highPTransact, midPTransact] lowTransact)," 
                                   ([highPTransact,midPTransact,lowPTransact]) 
                                   (addPC [highPTransact, midPTransact] lowPTransact)
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


                           
findFIntGood1 = TestCase ( assertEqual "for (findFInt [(1,ownerTransact),(2,defTransact),(3,defTransact)] \"foo\")"
                           (Just (1,ownerTransact),[(2,defTransact),(3,defTransact)])
                           (findFInt [(1,ownerTransact),(2,defTransact),(3,defTransact)] "foo")
                        )
                        
findFIntGood2 = TestCase ( assertEqual "for (findFInt [(1,defTransact),(2,ownerTransact),(3,defTransact)] \"foo\")"
                           (Just (2,ownerTransact),[(1,defTransact),(3,defTransact)])
                           (findFInt [(1,defTransact),(2,ownerTransact),(3,defTransact)] "foo")
                        )
                           
findFIntGood3 = TestCase ( assertEqual "for (findFInt [(1,defTransact),(2,defTransact),(3,ownerTransact)] \"foo\")"
                           (Just (3,ownerTransact),[(1,defTransact),(2,defTransact)])
                           (findFInt [(1,defTransact),(2,defTransact),(3,ownerTransact)] "foo")
                        )
                        
findFIntBad = TestCase ( assertEqual "for (findFInt [(1,defTransact),(2,defTransact),(3,defTransact)] \"foo\")"
                         (Nothing,[(1,defTransact),(2,defTransact),(3,defTransact)])
                         (findFInt [(1,defTransact),(2,defTransact),(3,defTransact)] "foo")
                       )
                       
findCIntGood1 = TestCase ( assertEqual "for (findCInt [ownerTransact,defTransact,defTransact] \"foo\")"
                           (Just ownerTransact,[defTransact,defTransact])
                           (findCInt [ownerTransact,defTransact,defTransact] "foo")
                         )
                         
findCIntGood2 = TestCase ( assertEqual "for (findCInt [defTransact,ownerTransact,defTransact] \"foo\")"
                           (Just ownerTransact,[defTransact,defTransact])
                           (findCInt [defTransact,ownerTransact,defTransact] "foo")
                         )
                           
findCIntGood3 = TestCase ( assertEqual "for (findCInt [defTransact,defTransact,ownerTransact] \"foo\")"
                           (Just ownerTransact,[defTransact,defTransact])
                           (findCInt [defTransact,defTransact,ownerTransact] "foo")
                         )
                           
findCIntBad = TestCase ( assertEqual "for (findCInt [defTransact,defTransact,defTransact] \"foo\")"
                           (Nothing,[defTransact,defTransact,defTransact])
                           (findCInt [defTransact,defTransact,defTransact] "foo")
                       )

findIntFEC = TestCase ( assertEqual "for (findInt [(1,ownerTransact),(2,defTransact),(3,defTransact)] [] \"foo\")"
                                    (((Just 1, ownerTransact),[(2,defTransact),(3,defTransact)],[]))
                                    (findInt [(1,ownerTransact),(2,defTransact),(3,defTransact)] [] "foo")

                      )

findIntCEC = TestCase ( assertEqual "for (findInt [(1,ownerTransact),(2,defTransact),(3,defTransact)] [] \"foo\")"
                                    ((Nothing, ownerTransact),[],[defTransact,defTransact])
                                    (findInt [] [ownerTransact,defTransact,defTransact] "foo")

                      )
                           
emptyAddIC = TestCase ( assertEqual "for (addIC [] (Nothing,defTransact))"
                                    ([(Nothing,defTransact)])
                                    (addIC [] (Nothing,defTransact)) 
                      )
                           
chainsTests = TestList [TestLabel "emptyFEC" emptyFEC,
                        TestLabel "middleAddFE" middleAddFE,
                        TestLabel "nearestAddFE" nearestAddFE,
                        TestLabel "lastAddFE" lastAddFE,
                        TestLabel "multyAddFE" multyAddFE,
                        TestLabel "emptyPC" emptyPC,
                        TestLabel "middleAddPC" middleAddPC,
                        TestLabel "nearestAddPC" nearestAddPC,
                        TestLabel "lastAddPC" lastAddPC,
                        TestLabel "multyAddPC" multyAddPC,
                        TestLabel "findFIntGood1" findFIntGood1,
                        TestLabel "findFIntGood2" findFIntGood2,
                        TestLabel "findFIntGood3" findFIntGood3,
                        TestLabel "findFIntBad" findFIntBad,
                        TestLabel "findCIntGood1" findCIntGood1,
                        TestLabel "findCIntGood2" findCIntGood2,
                        TestLabel "findCIntGood3" findCIntGood3,
                        TestLabel "findCIntBad" findCIntBad,
                        TestLabel "findIntFEC" findIntFEC,
                        TestLabel "findIntCEC" findIntCEC,
                        TestLabel "emptyAddIC" emptyAddIC
                        
                       ]
