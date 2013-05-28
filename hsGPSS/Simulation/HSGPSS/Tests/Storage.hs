module Simulation.HSGPSS.Tests.Storage (sStorageTests) where

import Test.HUnit
import Simulation.HSGPSS.Storage
import Data.List
import qualified Control.Exception as E

newtype ApproxSStorage = ASS SStorage deriving Show

instance Eq ApproxSStorage where
    (ASS (SStorage c1 un1 a1 uc1 u1 m1 l1)) == (ASS (SStorage c2 un2 a2 uc2 u2 m2 l2)) =
        (c1 == c2) && (un1 == un2) && (abs(a1 - a2) < 0.000001) && (uc1 == uc2) && 
        (abs(u1 - u2) < 0.000001) && (m1 == m2) && (abs(l1 - l2) < 0.000001)
        
goodSeq = [enter 2 1,
           enter 1 2,
           leave 2 3,
           enter 4 4,
           leave 1 5,
           leave 4 6,
           enter 3 7,
           enter 2 8,
           leave 3 9,
           leave 2 10]
           
goodSeqTest = TestCase (assertEqual "for (snd $ mapAccumL (\a f -> let r = f a in (r,ASS r)) (stInit 5) goodSeq),"
                        [ASS $ SStorage 5 3 0 2 0 2 1,
                         ASS $ SStorage 5 2 1 3 0.2 3 2,
                         ASS $ SStorage 5 4 (5/3) 3 (1/3) 3 3,
                         ASS $ SStorage 5 0 (1.5) 7 0.3 5 4,
                         ASS $ SStorage 5 1 2.2 7 0.44 5 5,
                         ASS $ SStorage 5 5 2.5 7 0.5 5 6,
                         ASS $ SStorage 5 2 (15/7) 10 (3/7) 5 7,
                         ASS $ SStorage 5 0 (9/4) 12 0.45 5 8,
                         ASS $ SStorage 5 3 (23/9) 12 (23/45) 5 9,
                         ASS $ SStorage 5 5 2.5 12 0.5 5 10
                        ]
                        (snd $ mapAccumL (\a f -> let r = f a in (r,ASS r)) (stInit 5) goodSeq)
                       )
                       
emptyEnterTest = TestCase (E.catch (do let r = enter 5 1 $ stInit 3
                                       assertFailure ("expected error but got " ++ show r))
                                   ((\err -> assertString "" ):: E.ErrorCall -> IO ())
                          )
                       
fullLeaveTest = TestCase (E.catch (do let r = leave 5 1 $ stInit 3
                                      assertFailure ("expected error but got " ++ show r))
                                  ((\err -> assertString "" ):: E.ErrorCall -> IO ())
                         )

                       
sStorageTests = TestList
 [TestLabel "goodSeqTest" goodSeqTest,
  TestLabel "emptyEnterTest" emptyEnterTest,
  TestLabel "fullLeaveTest" fullLeaveTest]
