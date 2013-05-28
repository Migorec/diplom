module Simulation.HSGPSS.Storage where


data SStorage = SStorage { capacity :: Int,
                           unused :: Int,
                           --inUse :: Int,
                           avgInUse :: Double,
                           useCount :: Int,
                           utilization :: Double,
                           maxInUse :: Int
                         } deriving (Eq, Show)
                         
               
stInit :: Int -> SStorage
stInit c = SStorage c c 0 0 0 0
                         
inUse :: SStorage -> Int
inUse sst = capacity sst - unused sst
                           
