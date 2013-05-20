module Simulation.HSGPSS.Tests.Facility (sFacilityTests) where

import Test.HUnit
import Simulation.HSGPSS.Facility
import Data.List
import qualified Control.Exception as E

newtype ApproxSFacility = ASF SFacility deriving Show

instance Eq ApproxSFacility where
    (ASF (SFacility iA1 cc1 ct1 lcc1 u1 _)) == (ASF (SFacility iA2 cc2 ct2 lcc2 u2 _)) =
        (iA1==iA2) && (cc1 == cc2) && (abs (ct1 - ct2) < 0.000001) && (abs (lcc1 - lcc2) < 0.000001) && (abs (u1 - u2) < 0.000001)
        
testSeq = [capture 1, release 2, capture 3, release 5, capture 6, release 7, capture 9, release 10]


goodSeq = TestCase (assertEqual "for snd $ mapAccumL (\a f -> let r = f a in (r,ASF r)) initFacility testSeq),"
                                [ASF $ SFacility False 1 0 1 0 [],
                                 ASF $ SFacility True 1 1 1 0.5 [],
                                 ASF $ SFacility False 2 1 3 (1/3) [],
                                 ASF $ SFacility True 2 3 3 (3/5) [],
                                 ASF $ SFacility False 3 3 6 (3/6) [],
                                 ASF $ SFacility True 3 4 6 (4/7) [],
                                 ASF $ SFacility False 4 4 9 (4/9) [],
                                 ASF $ SFacility True 4 5 9 0.5 []
                                ]
                                (snd $ mapAccumL (\a f -> let r = f a in (r,ASF r)) initFacility testSeq)
                  )
                  
sFacilityTests = TestList [TestLabel "goodSeq" goodSeq]
