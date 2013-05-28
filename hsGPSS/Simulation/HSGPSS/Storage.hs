module Simulation.HSGPSS.Storage where


data SStorage = SStorage { capacity :: Int,
                           unused :: Int,
                           avgInUse :: Double,
                           useCount :: Int,
                           utilization :: Double,
                           maxInUse :: Int,
                           lastMod :: Double
                         } deriving (Eq, Show)
                         
               
stInit :: Int -> SStorage
stInit c = SStorage c c 0 0 0 0 0
                         
inUse :: SStorage -> Int
inUse sst = capacity sst - unused sst
                           
                           
enter :: Int -> Double -> SStorage  -> SStorage
enter dec t sst = 
    if (unused sst < dec)
    then error "not enough units in storage!"
    else let dt = t - lastMod sst
             unused' = unused sst - dec
             inUse' = capacity sst - unused'
             avgInUse' = (avgInUse sst * lastMod sst + (fromIntegral $ inUse sst) * dt) / t
         in sst{unused = unused',
                useCount = useCount sst + dec,
                avgInUse = avgInUse',
                maxInUse = max (maxInUse sst) inUse',
                utilization = avgInUse' / (fromIntegral $ capacity sst),
                lastMod = t
               } 
               
leave :: Int -> Double -> SStorage  -> SStorage
leave inc t sst = 
    if (capacity sst < unused sst + inc)
    then error "overfull storage!"
    else let dt = t - lastMod sst
             unused' = unused sst + inc
             inUse' = capacity sst - unused'
             avgInUse' = (avgInUse sst * lastMod sst + (fromIntegral $ inUse sst) * dt) / t
         in sst{unused = unused',
                avgInUse = avgInUse',
                utilization = avgInUse' / (fromIntegral $ capacity sst),
                lastMod = t
               }

