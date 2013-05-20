module Simulation.HSGPSS.Simulate where

    
import Data.Map hiding ((!), foldl, map)
import Data.Array
import Data.Foldable
import GHC.Exts (sortWith)
import Prelude hiding (foldl)
import Simulation.HSGPSS.Blocks hiding (blocks)
import Simulation.HSGPSS.Chains
import Simulation.HSGPSS.Transaction
import Simulation.HSGPSS.Facility hiding (release, queue)
import Simulation.HSGPSS.Queue
import Simulation.HSGPSS.SimulationState
import Simulation.HSGPSS.Simulate.Generate
import Simulation.HSGPSS.Simulate.Advance
import Simulation.HSGPSS.Simulate.Queue
import Simulation.HSGPSS.Simulate.Transfer
import Simulation.HSGPSS.Simulate.Seize
import Debug.Trace


simulate :: BlockStateMonad -> Int -> IO SimulationState
simulate bs tt = firstGenerate (ssInit bs tt) >>= fecStep

cecStep :: SimulationState -> IO SimulationState
cecStep ss = 
    case cec ss of
      [] -> {-trace (show ss ++ "\n\n") $ -}fecStep ss
      t:ts -> {-trace (show ss ++ "\n\n") $ -}do ns <- moveTransaction t ss{cec = ts}
                                                 if toTerminate ns > 0
                                                  then cecStep ns
                                                  else return ns
      
moveTransaction :: Transaction -> SimulationState -> IO SimulationState
moveTransaction t ss' = 
 do let sblock = blocks ss' ! nextBlock t 
        pblock = blocks ss' ! currentBlock t 
    ss <- generate ss' pblock               
    case block sblock of
        AdvanceRange _ _ -> advance ss sblock t
        AdvanceFunc _ _ -> advance ss sblock t
        Queue _ _ -> queue ss sblock t
        Depart _ _ -> depart ss sblock t
        Terminate dec -> return $ ss {toTerminate = toTerminate ss - dec}
        TransferUnconditional _ -> transfer ss sblock t
        TransferFractional1 _ _ -> transfer ss sblock t
        TransferFractional2 _ _ _ -> transfer ss sblock t
        TransferBoth _ _ -> transfer ss sblock t
        TransferAll _ _ _ -> transfer ss sblock t
        Seize _ -> seize ss sblock t
        Release _ -> release ss sblock t
        GenerateRangeNoLimit _ _ _ _ -> error "Transaction can't enter GENERATE block!"
        GenerateRangeGeneral _ _ _ _ _ -> error "Transaction can't enter GENERATE block!"
        GenerateFuncGeneral _ _ _ _ _ -> error "Transaction can't enter GENERATE block!"
        GenerateFuncNoLimit _ _ _ _ -> error "Transaction can't enter GENERATE block!"
        _ -> undefined  
        
      
fecStep :: SimulationState -> IO SimulationState
fecStep ss = 
    case fec ss of
        [] -> return ss
        (time,transact):ts -> let ces = transact : (map snd $ takeWhile (\(t,_) -> t <= time) ts)
                              in cecStep ss{currentTime = time, 
                                            cec = foldl addPC (cec ss) ces, 
                                            fec = dropWhile (\(t,_) -> t <= time) ts}

firstGenerate :: SimulationState -> IO SimulationState
firstGenerate ss = do let bs = blocks ss
                      foldlM generate ss bs 

