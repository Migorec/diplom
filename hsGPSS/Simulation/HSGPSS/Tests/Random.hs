module Simulation.HSGPSS.Tests.Random (randomTests) where

import Test.HUnit
import Simulation.HSGPSS.Random
import Simulation.HSGPSS.Random.Functions
import Data.Vector hiding (zip, filter, replicate, length, map, replicateM)
import Control.Monad
import Statistics.Test.ChiSquared
import Statistics.Distribution
import Statistics.Distribution.Exponential
import Data.List (mapAccumL)
import Debug.Trace
 

statUniform m h = do let a = m - h
                         b = m + h
                     l <- replicateM 200 $ randomMHTime m h
                     let dx = (b - a) / 10
                         hist = map (\i -> length $ filter (\x -> x > a + i*dx && x <= a + (i+1)*dx) l) [0 .. 9]
                     return $ fromList $ zip hist $ replicate 10 20

mhTimeTest = TestCase (do stat <- statUniform 35 7
                          case chi2test 0.05 0 stat of
                            Significant -> assertFailure "mhRandomTime test failed with p-value = 0.05"
                            NotSignificant -> assertString ""
                      )
                    
nonRandomTimeTest = TestCase ( do rs <- replicateM 200 $ randomMHTime 10 0
                                  if (length $ filter (/=10) rs) == 0
                                    then assertString ""
                                    else assertFailure "nonRandomTimeTest failed"
                             ) 
                      
statDistUniform d1 d2 = do l <- replicateM 200 $ randomDist d1 d2
                           let hist = map (\i -> length $ filter (\x -> x == (d1 + i)) l) [0 ..  9] 
                           return $ fromList $ zip hist $ replicate 10 20
                
randomDistTest = TestCase ( do stat <- statDistUniform 2 11
                               case chi2test 0.05 0 stat of 
                                    Significant -> assertFailure "randomDist test failed with p-value = 0.05"
                                    NotSignificant -> assertString ""
                          )
                          
nonRandomDistTest = TestCase ( do rs <-replicateM 200 $ randomDist 5 5
                                  if (length $ filter (/=5) rs) == 0
                                    then assertString ""
                                    else assertFailure "nonRandomDistTest failed"
                             )
  
  
xpd lambda x | x < 0 = 0
             | otherwise = 1 - exp (-lambda * x)
  
xpp lambda x1 x2 = xpd lambda x2 - xpd lambda x1
  
statXP m = do let a = 0
                  b = m * 4
                  dx = (b - a) / 10
                  lambda = 1/m
              l <- replicateM 200 $ randomMFTime m xpdis
              let hist = map (\i -> length $ filter (\x -> x > a + i*dx && x <= a + (i+1)*dx) l) [0 .. 9]
                  hist1 = map (\i -> 200 * xpp lambda (a + i*dx) (a + (i+1)*dx)) [0..9]
              return $ fromList $ zip hist hist1

xpdisTest1 = TestCase (do stat <- statXP 1
                          case chi2test 0.05 0 stat of
                            Significant -> assertFailure "xpdisTest1 test failed with p-value = 0.05"
                            NotSignificant -> assertString ""
                      )

xpdisTest5 = TestCase (do stat <- statXP 5
                          case chi2test 0.05 0 stat of
                            Significant -> assertFailure "xpdisTest5 test failed with p-value = 0.05"
                            NotSignificant -> assertString ""
                      )

                      
randomTests = TestList [TestLabel "mhTimeTest" mhTimeTest,
                        TestLabel "nonRandomTimeTest" nonRandomTimeTest,
                        TestLabel "randomDistTest" randomDistTest,
                        TestLabel "nonRandomDistTest" nonRandomDistTest,
                        TestLabel "xpdisTest1" xpdisTest1,
                        TestLabel "xpdisTest5" xpdisTest5
                       ]
