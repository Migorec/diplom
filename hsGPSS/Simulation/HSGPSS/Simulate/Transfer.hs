module Simulation.HSGPSS.Simulate.Transfer where

import Simulation.HSGPSS.Blocks hiding (blocks)
import Simulation.HSGPSS.SimulationState
import Simulation.HSGPSS.Chains
import Simulation.HSGPSS.Transaction
import qualified Simulation.HSGPSS.Facility as F
import Simulation.HSGPSS.MyArray
import Data.Array 
import System.Random
import Data.Map as DM hiding ((!))

isAvailable :: SimulationState -> Block -> Transaction -> Bool
isAvailable _ (GenerateRangeGeneral _ _ _ _ _) _ = False
isAvailable _ (GenerateRangeNoLimit _ _ _ _) _ = False
isAvailable _ (GenerateFuncGeneral _ _ _ _ _) _ = False
isAvailable _ (GenerateFuncNoLimit _ _ _ _) _ = False
isAvailable ss (Seize name) transact = 
    case DM.lookup name $ facilities ss of
        Nothing -> True
        Just f -> F.isAvailable f
isAvailable ss (PreemptIR name _ _ _) transact = undefined
isAvailable ss (PreemptPR name _ _ _) transact = undefined
isAvailable _ _ _ = True 

transfer :: SimulationState -> SBlock -> Transaction -> IO SimulationState
transfer ss (SBlock (TransferUnconditional dest) ix) transact = return $ ss {cec = transact{currentBlock = ix, nextBlock = dest} : cec ss}

transfer ss (SBlock (TransferFractional1 p dest) ix) transact =
    do rVal <- randomRIO (0, 1)
       if rVal < p
        then return $ ss {cec = transact{currentBlock = ix, nextBlock = dest} : cec ss}
        else return $ ss {cec = transact{currentBlock = ix, nextBlock = ix + 1} : cec ss}
       
transfer ss (SBlock (TransferFractional2 p dest1 dest2) ix) transact =
    do rVal <- randomRIO (0, 1)
       if rVal < p
        then return $ ss {cec = transact{currentBlock = ix, nextBlock = dest1} : cec ss}
        else return $ ss {cec = transact{currentBlock = ix, nextBlock = dest2} : cec ss}
   
transfer ss (SBlock (TransferBoth dest1 dest2) ix) transact =
    if isAvailable ss (block $ blocks ss ! dest1) transact
    then return $ ss {cec = transact{currentBlock = ix, nextBlock = dest1} : cec ss}
    else if isAvailable ss (block $ blocks ss ! dest2) transact
         then return $ ss {cec = transact{currentBlock = ix, nextBlock = dest1} : cec ss}
         else case fec ss of
                (t,_):xs -> return $ ss {fec = addFE (fec ss) (t,transact{state = Suspended})}
                [] -> return $ ss {fec = [(currentTime ss, transact{state = Suspended})]}
   
transfer ss (SBlock (TransferAll dest1 dest2 step) ix) transact 
    | dest1 > dest2 = case fec ss of
                (t,_):xs -> return $ ss {fec = addFE (fec ss) (t,transact{state = Suspended})}
                [] -> return $ ss {fec = [(currentTime ss, transact{state = Suspended})]}
    | otherwise = if isAvailable ss (block $ blocks ss ! dest1) transact
                  then return $ ss {cec = transact{currentBlock = ix, nextBlock = dest1} : cec ss}
                  else transfer ss (SBlock (TransferAll (dest1 + step) dest2 step) ix) transact
                
transfer ss (SBlock (TransferPick dest1 dest2) ix) transact = 
    do rDest <- randomRIO (dest1, dest2) -- TEST!!!
       return $ ss {cec = transact{currentBlock = ix, nextBlock = rDest} : cec ss}
       
--transfer ss (SBlock (TransferParameter param inc) _ ix) transact = 
-- placemaker должен быть int

--тоже с subroutine

