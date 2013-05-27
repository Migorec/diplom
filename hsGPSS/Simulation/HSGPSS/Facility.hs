module Simulation.HSGPSS.Facility where

import Simulation.HSGPSS.Chains
import Simulation.HSGPSS.Transaction

data SFacility = SFacility { isAvailable :: Bool,
                             isInterrupted :: Bool,
                             captureCount :: Int,
                             captureTime :: Double, --суммарно время, в течении которого устройство было занято
                             lastCaptureTime :: Double, -- момент времени, когда устройство стало занято в последний раз
                             utilization :: Double,
                             dc :: DC, --delay chain
                             pc :: DC, --pending chain (попытались вытеснить, но не смогли)
                             ic :: IC --interrupt chains
                           } deriving (Eq, Show)
                           
initFacility :: SFacility
initFacility = SFacility True False 0 0 0 0 [] [] []

queue :: Transaction -> SFacility -> SFacility
queue t sf = sf{dc = addPC (dc sf) t}

sInterrupt :: (Maybe Double, Transaction) -> SFacility -> SFacility
sInterrupt r sf = sf{ic = addIC (ic sf) r}

setInterrupt :: SFacility -> SFacility
setInterrupt = sf{isInterrupted = True}

usetInterrupt :: SFacility -> SFacility
usetInterrupt = sf{isInterrupted = False}


pend :: Transaction -> SFacility -> SFacility
pend t sf = sf{pc = addPC (pc sf) t}

capture :: Double -> SFacility -> SFacility
capture t sf = sf {isAvailable = False, 
                   captureCount = 1 + captureCount sf, 
                   lastCaptureTime = t,
                   utilization = captureTime sf / t
                  }


release :: Double -> SFacility -> SFacility
release t sf = let newCaptureTime = captureTime sf + t - lastCaptureTime sf
               in sf {isAvailable = True, 
                      captureTime = newCaptureTime, 
                      utilization = newCaptureTime / t
                     }

