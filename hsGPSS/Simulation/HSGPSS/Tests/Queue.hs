module Simulation.HSGPSS.Tests.Queue (sQueueTests) where

import Test.HUnit
import Simulation.HSGPSS.Queue 
import Data.List
import qualified Control.Exception as E

newtype ApproxSQueue = ASQ SQueue deriving Show


instance Eq ApproxSQueue where
    (ASQ (SQueue cc1 mc1 ac1 lt1)) == (ASQ (SQueue cc2 mc2 ac2 lt2)) = (cc1==cc2) && 
                                                                       (mc1==mc2) && 
                                                                       (abs (ac1 - ac2) < 0.000001) &&
                                                                       (abs (lt1 - lt2) < 0.000001)

testSeq = [modify 1 1,
           modify 2 (-1),
           modify 3 1,
           modify 4 1,
           modify 5 (-2),
           modify 6 2,
           modify 7 (-1),
           modify 8 1,
           modify 9 (-2)]
           
badTestSeq = [modify 1 1,
              modify 2 (-1),
              modify 3 1,
              modify 4 1,
              modify 5 (-3),
              modify 6 2,
              modify 7 (-1),
              modify 8 1,
              modify 9 (-2)]
           


goodSeq = TestCase (assertEqual "for (snd $ mapAccumL (\a f -> let r = f a in (r,r)) initQueue testSeq),"
                                [ASQ $ SQueue 1 1 0 1,
                                 ASQ $ SQueue 0 1 0.5 2,
                                 ASQ $ SQueue 1 1 (1/3) 3,
                                 ASQ $ SQueue 2 2 0.5 4,
                                 ASQ $ SQueue 0 2 (4/5) 5,
                                 ASQ $ SQueue 2 2 (2/3) 6,
                                 ASQ $ SQueue 1 2 (6/7) 7,
                                 ASQ $ SQueue 2 2 (7/8) 8,
                                 ASQ $ SQueue 0 2 1 9
                                ]
                                (snd $ mapAccumL (\a f -> let r = f a in (r,ASQ r)) initQueue testSeq)
                  )
                  
emptyDepart = TestCase (E.catch (do let r = modify 1 (-1) initQueue
                                    assertFailure ("expected error but got " ++ show r))
                                ((\err -> assertString "" ):: E.ErrorCall -> IO ())
                       )

badSeq = TestCase (E.catch (do let r = snd $ mapAccumL (\a f -> let r = f a in (r,ASQ r)) initQueue badTestSeq
                               assertFailure ("expected error but got " ++ show r))
                           ((\err -> assertString "" ):: E.ErrorCall -> IO ())
                       )

                  

sQueueTests = TestList [TestLabel "goodSeq" goodSeq,
                        TestLabel "emptyDepart" emptyDepart,
                        TestLabel "badSeq" badSeq]
