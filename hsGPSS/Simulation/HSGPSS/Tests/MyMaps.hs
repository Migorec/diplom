module Simulation.HSGPSS.Tests.MyMaps (myMapsTests) where

import Test.HUnit
import Simulation.HSGPSS.MyMaps
import Simulation.HSGPSS.Facility
import Simulation.HSGPSS.Queue
import Data.Map


emptyFacilityMap = TestCase (assertEqual "for (defaultUpdate id \"key\" empty)," 
                                   (fromList [("key", initFacility)]) 
                                   (defaultUpdate id "key" empty :: Map String SFacility)
                           )
     
initFacilityMap :: Map String SFacility
initFacilityMap = fromList [("key", initFacility)]
                           
existFacilityMap = TestCase (assertEqual "for (defaultUpdate (\\f -> f{isAvailable = True}) \"key\" initFacilityMap)," 
                                   (fromList [("key", initFacility{isAvailable = True})]) 
                                   (defaultUpdate (\f -> f{isAvailable = True}) "key" initFacilityMap :: Map String SFacility)
                           )
                           
notExistFacilityMap = TestCase (assertEqual "for (defaultUpdate (\\f -> f{isAvailable = True}) \"key1\" initFacilityMap)," 
                                   (fromList [("key", initFacility),("key1", initFacility{isAvailable = True})]) 
                                   (defaultUpdate (\f -> f{isAvailable = True}) "key1" initFacilityMap :: Map String SFacility)
                           )
                           
emptyQueueMap = TestCase (assertEqual "for (defaultUpdate id \"key\" empty)," 
                                   (fromList [("key", initQueue)]) 
                                   (defaultUpdate id "key" empty :: Map String SQueue)
                           )
             
initQueueMap :: Map String SQueue
initQueueMap = fromList [("key", initQueue)]              
  
existQueueMap = TestCase (assertEqual "for (defaultUpdate (\\f -> f{currentContent = 1}) \"key\" initQueueMap)," 
                                   (fromList [("key", initQueue{currentContent = 1})]) 
                                   (defaultUpdate (\f -> f{currentContent = 1}) "key" initQueueMap :: Map String SQueue)
                           )
 
notExistQueueMap = TestCase (assertEqual "for (defaultUpdate (\\f -> f{currentContent = 1}) \"key1\" initFacilityMap)," 
                                   (fromList [("key", initQueue),("key1", initQueue{currentContent = 1})]) 
                                   (defaultUpdate (\f -> f{currentContent = 1}) "key1" initQueueMap :: Map String SQueue)
                           )
                           
myMapsTests = TestList [TestLabel "emptyFacilityMap" emptyFacilityMap,
                        TestLabel "existFacilityMap" existFacilityMap,
                        TestLabel "notExistFacilityMap" notExistFacilityMap,
                        TestLabel "emptyQueueMap" emptyQueueMap,
                        TestLabel "existQueueMap" existQueueMap,
                        TestLabel "notExistQueueMap" notExistQueueMap
                       ]
