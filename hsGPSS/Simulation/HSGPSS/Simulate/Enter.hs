module Simulation.HSGPSS.Simulate.Enter where

import Simulation.HSGPSS.Blocks hiding (blocks, priority, storages)
import Simulation.HSGPSS.SimulationState 
import Simulation.HSGPSS.Chains
import Simulation.HSGPSS.Transaction
import Simulation.HSGPSS.MyArray
import Simulation.HSGPSS.MyMaps
import Simulation.HSGPSS.Queue
import Simulation.HSGPSS.Storage as S
import Data.Map as DM hiding ((!))
import Data.Array ((!))


enter' :: SimulationState -> SBlock -> Transaction -> SimulationState
enter' ss (SBlock (Enter s count) ix) transact = 
    case DM.lookup s $ storages ss of
        Nothing -> error "trying to enter storage, that does not exist"
        Just st -> if unused st < count
                   then ss{storages = defaultUpdate (S.queue transact) s $ storages ss
                           } 
                   else ss{storages = defaultUpdate (S.enter count (currentTime ss)) s $ storages ss,
                           cec = transact{currentBlock = ix, nextBlock = ix + 1} : cec ss
                           }

leave' :: SimulationState -> SBlock -> Transaction -> SimulationState
leave' ss (SBlock (Leave s count) ix) transact =
    case DM.lookup s $ storages ss of
        Nothing -> error "trying to leave storage, that does not exist"
        Just st -> let fs = S.leave count (currentTime ss) st
                   in case dc fs of
                        [] -> ss{storages = defaultUpdate (\_ -> fs) s $ storages ss,
                                 cec = transact{currentBlock = ix, nextBlock = ix + 1} : cec ss}
                        (t:ts) -> let count' = dec $ block (blocks ss ! (nextBlock t)) in 
                                  if count' < unused fs
                                  then ss{storages = defaultUpdate (\_ -> S.enter count' (currentTime ss) fs{dc = ts}) s $ storages ss,
                                          cec = transact{currentBlock = ix, nextBlock = ix + 1} : 
                                                t{currentBlock = nextBlock t, nextBlock = 1 + nextBlock t} :
                                                cec ss
                                         }
                                  else ss{storages = defaultUpdate (\_ -> fs) s $ storages ss,
                                          cec = transact{currentBlock = ix, nextBlock = ix + 1} : cec ss}
        
        

enter :: SimulationState -> SBlock -> Transaction -> IO SimulationState
enter ss sb t = return $ enter' ss sb t

leave :: SimulationState -> SBlock -> Transaction -> IO SimulationState
leave ss sb t = return $ leave' ss sb t
