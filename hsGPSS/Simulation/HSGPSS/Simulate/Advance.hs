module Simulation.HSGPSS.Simulate.Advance  where

import Simulation.HSGPSS.Blocks hiding (blocks)
import Simulation.HSGPSS.SimulationState
import Simulation.HSGPSS.Chains
import Simulation.HSGPSS.Transaction
import Simulation.HSGPSS.MyArray
import Data.IntMap
import Simulation.HSGPSS.Random

advance' :: SimulationState -> Int -> Transaction -> Double -> SimulationState
advance' ss ix transact t = let nt = transact{currentBlock = ix, nextBlock = ix + 1, state = Passive}
                            in ss{fec = addFE (fec ss) (currentTime ss + t, nt)}


advance :: SimulationState -> SBlock -> Transaction -> IO SimulationState
advance ss (SBlock (AdvanceRange m h) ix) transact = 
    do rTime <- randomMHTime m h
       return $ advance' ss ix transact rTime
advance ss (SBlock (AdvanceFunc m f) ix) transact = 
    do rTime <- randomMFTime m f
       return $ advance' ss ix transact rTime
