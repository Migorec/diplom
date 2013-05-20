module Simulation.HSGPSS.Queue where

data SQueue = SQueue { currentContent :: Int,
                       maximumContent :: Int,
                       averageContent :: Double,
                       lastChangeTime :: Double
                     } deriving (Eq, Show)
                      
initQueue = SQueue 0 0 0 0

modify :: Double -> Int -> SQueue -> SQueue
modify time inc (SQueue cc mc ac lt) = 
    if newCount < 0 
    then error "There are no enough entities in queue"
    else SQueue newCount 
                (max mc newCount) 
                ((ac * lt + fromIntegral cc * dt) / time) 
                time
    where newCount = cc + inc
          dt = time - lt
