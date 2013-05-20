module Simulation.HSGPSS.Simulate.Queue where

import Simulation.HSGPSS.Blocks hiding (blocks)
import Simulation.HSGPSS.SimulationState
import Simulation.HSGPSS.Chains
import Simulation.HSGPSS.Transaction
import Simulation.HSGPSS.MyArray
import Simulation.HSGPSS.MyMaps
import Simulation.HSGPSS.Queue
import Data.IntMap
import System.Random

queue' :: SimulationState -> SBlock -> Transaction -> SimulationState
queue' ss (SBlock (Queue name inc) ix) transact = ss{queues = defaultUpdate (modify (currentTime ss) inc) name $ queues ss, cec = transact{currentBlock = ix, nextBlock = ix + 1} : cec ss}

queue :: SimulationState -> SBlock -> Transaction -> IO SimulationState
queue ss sb transact = return $ queue' ss sb transact

depart' :: SimulationState -> SBlock -> Transaction -> SimulationState
depart' ss (SBlock (Depart name dec) ix) transact = ss{queues = defaultUpdate (modify (currentTime ss) (-dec)) name $ queues ss, cec = transact{currentBlock = ix, nextBlock = ix + 1} : cec ss}

depart :: SimulationState -> SBlock -> Transaction -> IO SimulationState
depart ss sb transact = return $ depart' ss sb transact
