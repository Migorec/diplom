module Simulation.HSGPSS.Tests.MyMaps (myMapsTests) where

import Test.HUnit
import Simulation.HSGPSS.MyMaps
import Simulation.HSGPSS.Facility
import Simulation.HSGPSS.Queue
import Simulation.HSGPSS.Storage
import Data.Map
import Data.IntMap hiding (fromList,empty)
import qualified Data.IntMap as IM (fromList)
import qualified Control.Exception as E


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

initStorageMap :: Map String SStorage
initStorageMap = fromList [("key", stInit 5)]
                           
existStorageMap = TestCase (assertEqual "for (defaultUpdate (\f -> f{maxInUse = 4}) \"key\" initStorageMap :: Map String SFacility)," 
                                   (fromList [("key", (stInit 5) {maxInUse = 4})]) 
                                   (defaultUpdate (\f -> f{maxInUse = 4}) "key" initStorageMap :: Map String SStorage)
                           )
  
notExistStorageMap = TestCase (E.catch (do let r = (defaultUpdate (\f -> f{maxInUse = 4}) "key1" initStorageMap :: Map String SStorage)
                                           assertFailure ("expected error but got " ++ show r))
                                   ((\err -> assertString "" ):: E.ErrorCall -> IO ())
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
                           
initProps :: IntMap Double
initProps = IM.fromList [(1,1)]

existPropMap = TestCase (assertEqual "for (defaultUpdate (+1) 1 initProps :: IntMap Double)," 
                                   (IM.fromList [(1,2)]) 
                                   (defaultUpdate (+1) 1 initProps :: IntMap Double)
                           )
 
notExistPropMap = TestCase (assertEqual "for (defaultUpdate (+1) 2 initProps :: IntMap Double)," 
                                   (IM.fromList [(1,1),(2,1)]) 
                                   (defaultUpdate (+1) 2 initProps :: IntMap Double)
                           )


                           
myMapsTests = TestList [TestLabel "emptyFacilityMap" emptyFacilityMap,
                        TestLabel "existFacilityMap" existFacilityMap,
                        TestLabel "notExistFacilityMap" notExistFacilityMap,
                        TestLabel "emptyQueueMap" emptyQueueMap,
                        TestLabel "existQueueMap" existQueueMap,
                        TestLabel "notExistQueueMap" notExistQueueMap,
                        TestLabel "existStorageMap" existStorageMap,
                        TestLabel "notExistStorageMap" notExistStorageMap,
                        TestLabel "existPropMap" existPropMap,
                        TestLabel "notExistPropMap" notExistPropMap
                       ]
