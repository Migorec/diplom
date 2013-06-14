module Simulation.HSGPSS.Tests.Facility (sFacilityTests) where

import Test.HUnit
import Simulation.HSGPSS.Facility
import Simulation.HSGPSS.Transaction
import Data.IntMap
import Data.List
import qualified Control.Exception as E

newtype ApproxSFacility = ASF SFacility deriving Show

instance Eq ApproxSFacility where
    (ASF (SFacility iA1 iI1 cc1 ct1 lcc1 u1 op1 _ _ _)) == (ASF (SFacility iA2 iI2 cc2 ct2 lcc2 u2 op2 _ _ _)) =
        (iA1==iA2) && (iI1==iI2) && (cc1 == cc2) && (abs (ct1 - ct2) < 0.000001) && (abs (lcc1 - lcc2) < 0.000001) && (abs (u1 - u2) < 0.000001) && (op1 == op2)
        
testSeq = [capture 1 1, release 2, capture 3 2, release 5, capture 6 3, release 7, capture 9 4, release 10]


goodSeq = TestCase (assertEqual "for snd $ mapAccumL (\a f -> let r = f a in (r,ASF r)) initFacility testSeq),"
                                [ASF $ SFacility False False 1 0 1 0 1 [] [] [],
                                 ASF $ SFacility True False 1 1 1 0.5 0 [] [] [],
                                 ASF $ SFacility False False 2 1 3 (1/3) 2 [] [] [],
                                 ASF $ SFacility True False 2 3 3 (3/5) 0 [] [] [],
                                 ASF $ SFacility False False 3 3 6 (3/6) 3 [] [] [],
                                 ASF $ SFacility True False 3 4 6 (4/7) 0 [] [] [],
                                 ASF $ SFacility False False 4 4 9 (4/9) 4 [] [] [],
                                 ASF $ SFacility True False 4 5 9 0.5 0 [] [] []
                                ]
                                (snd $ mapAccumL (\a f -> let r = f a in (r,ASF r)) initFacility testSeq)
                  )

defTransact :: Transaction
defTransact = Transaction 0 0 0 Active empty ""

queueTest = TestCase ( assertEqual "for  (queue defTransact initFacility)"
                                   initFacility{dc = [defTransact]}
                                   (queue defTransact initFacility)
                     )
                  
sInterruptTest = TestCase ( assertEqual "for (sInterrupt (Nothing,defTransact) initFacility)"
                                   initFacility{ic = [(Nothing, defTransact)]}
                                   (sInterrupt (Nothing,defTransact) initFacility)
                     )
                  
setInterruptTest = TestCase ( assertEqual "for (setInterrupt initFacility)"
                                initFacility{isInterrupted = True}
                                (setInterrupt initFacility)
                            )
                  
unsetInterruptTest = TestCase ( assertEqual "for (unsetInterrupt initFacility)"
                                initFacility{isInterrupted = False}
                                (unsetInterrupt initFacility)
                            )
                  
pendTest = TestCase ( assertEqual "for  (pend defTransact initFacility)"
                                   initFacility{pc = [defTransact]}
                                   (pend defTransact initFacility)
                     )
                  
sFacilityTests = TestList [TestLabel "goodSeq" goodSeq,
                           TestLabel "queueTest" queueTest,
                           TestLabel "sInterruptTest" sInterruptTest,
                           TestLabel "setInterruptTest" setInterruptTest,
                           TestLabel "unsetInterruptTest" unsetInterruptTest,
                           TestLabel "pendTest" pendTest]
