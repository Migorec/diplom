module Simulation.HSGPSS.Simulate.Seize where

import Simulation.HSGPSS.Blocks hiding (blocks, priority)
import Simulation.HSGPSS.SimulationState hiding (dc)
import Simulation.HSGPSS.Chains
import Simulation.HSGPSS.Transaction
import Simulation.HSGPSS.MyArray
import Simulation.HSGPSS.MyMaps
import Simulation.HSGPSS.Queue
import Simulation.HSGPSS.Facility as F
import Data.Map as DM
import System.Random

seize' :: SimulationState -> SBlock -> Transaction -> SimulationState
seize' ss (SBlock (Seize f) ix) transact =
    if case DM.lookup f $ facilities ss of
        Nothing -> True
        Just sf -> isAvailable sf
    then ss {facilities = defaultUpdate (F.capture (currentTime ss) (priority transact)) f $ facilities ss,
             cec = transact{currentBlock = ix, nextBlock = ix + 1, ownership = f} : cec ss}
    else ss {facilities = defaultUpdate (F.queue transact{state = Passive}) f $ facilities ss}

seize :: SimulationState -> SBlock -> Transaction -> IO SimulationState
seize ss sb transact = return $ seize' ss sb transact
                      
release' :: SimulationState -> SBlock -> Transaction -> SimulationState
release' ss (SBlock (Release f) ix) transact = 
    case dc $ facilities ss ! f of
        [] -> ss{facilities = defaultUpdate (F.release (currentTime ss)) f $ facilities ss,
                 cec = transact{currentBlock = ix, nextBlock = ix + 1, ownership = ""} : cec ss}
        t:ts -> ss{facilities = defaultUpdate (\f -> (F.capture (currentTime ss) (priority t) $ F.release (currentTime ss) f){dc = ts}) f $ facilities ss,
                   cec = transact{currentBlock = ix, nextBlock = ix + 1, ownership = ""} : t{nextBlock = nextBlock t + 1, currentBlock = nextBlock t, state = Active, ownership = f} : cec ss}
                      
release :: SimulationState -> SBlock -> Transaction -> IO SimulationState
release ss sb transact = return $ release' ss sb transact
   


