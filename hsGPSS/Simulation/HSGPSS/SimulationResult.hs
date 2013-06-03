module Simulation.HSGPSS.SimulationResult where

import Simulation.HSGPSS.SimulationState hiding (facilities, storages)
import Simulation.HSGPSS.Facility as F
import Simulation.HSGPSS.Queue
import Simulation.HSGPSS.Storage as S
import Data.Map as DM

data SimulationResult = SimulationResult {facilities :: Map String SFacility,
                                          storages :: Map String SStorage,
                                          queues :: Map String SQueue,
                                          simulationTime :: Double
                                         } deriving (Eq, Show)
                                         
result :: SimulationState -> SimulationResult
result (SimulationState _ _ _ fs qs ss _ t) = SimulationResult fs ss qs t

fUtilisation  :: SimulationResult -> String -> Maybe Double
fUtilisation sr fn = do f <- DM.lookup fn $ facilities sr
                        return $ F.utilization f

fcCount :: SimulationResult -> String -> Maybe Int
fcCount sr fn = do f <- DM.lookup fn $ facilities sr
                   return $ F.captureCount f
                        
sUtilisation :: SimulationResult -> String -> Maybe Double
sUtilisation sr sn = do s <- DM.lookup sn $ storages sr
                        return $ S.utilization s
