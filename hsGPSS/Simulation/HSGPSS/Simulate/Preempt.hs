module Simulation.HSGPSS.Simulate.Preempt where

import Simulation.HSGPSS.Blocks
import Simulation.HSGPSS.Chains
import Simulation.HSGPSS.MyMaps
import Simulation.HSGPSS.Facility as F
import Simulation.HSGPSS.Transaction
import Simulation.HSGPSS.SimulationState
import Simulation.HSGPSS.MyArray
import Data.Map as DM

preempt' :: SimulationState -> SBlock -> Transaction -> SimulationState
preempt' ss (SBlock (PreemptIR f _ p False) ix) transact = 
    if case DM.lookup f $ facilities ss of
        Nothing -> True
        Just sf -> isAvailable sf
    then ss {facilities = defaultUpdate (F.capture (currentTime ss)) f $ facilities ss,
             cec = transact{currentBlock = ix, nextBlock = ix + 1, ownership = f} : cec ss}
    else if  isInterrupted $ facilities ss ! f
         then ss {facilities = defaultUpdate (pend transact) f $ facilities ss}
         else let ((mt,it),fec',cec') = findInt (fec ss) (cec ss) f
                  mt' = fmap (\x -> x - currentTime ss) mt 
                  it' = case (p,mt') of
                         (Nothing,_) -> it{ownership=""}
                         (_,Nothing) -> it{ownership=""}
                         (Just n, Just t) -> it {params = defaultUpdate (\_ -> t) n $ params it, ownership=""}
                  t = currentTime ss
              in ss {facilities = defaultUpdate (\f -> F.setInterrupt $
                                                       F.sInterrupt (mt',it') $ 
                                                       F.capture t $ 
                                                       F.release t f) f $ facilities ss, 
                     fec = fec',
                     cec = transact {currentBlock = ix, nextBlock = ix + 1, ownership = f}: cec'     
                    }
                    
preempt' _ (SBlock (PreemptIR _ Nothing _ True) _) _ = error "C Operand in PREEMPT block must be specified if E Operant is RE"

preempt' ss (SBlock (PreemptIR f d p True) ix) transact =
    if case DM.lookup f $ facilities ss of
        Nothing -> True
        Just sf -> isAvailable sf
    then ss {facilities = defaultUpdate (F.capture (currentTime ss)) f $ facilities ss,
             cec = transact{currentBlock = ix, nextBlock = ix + 1, ownership = f} : cec ss}
    else if  isInterrupted $ facilities ss ! f
         then ss {facilities = defaultUpdate (pend transact) f $ facilities ss}
         else let ((mt,it),fec',cec') = findInt (fec ss) (cec ss) f
                  mt' = fmap (\x -> x - currentTime ss) mt
                  it' = case (p,mt') of
                         (Nothing,_) -> it{ownership=""}
                         (_,Nothing) -> it{ownership=""}
                         (Just n, Just t) -> it {params = defaultUpdate (\_ -> t) n $ params it, ownership=""}
                  t = currentTime ss
              in ss {facilities = defaultUpdate (\f -> F.setInterrupt $
                                                       F.capture t $ 
                                                       F.release t f) f $ facilities ss, 
                     fec = fec',
                     cec = transact {currentBlock = ix, nextBlock = ix + 1, ownership = f}:it':cec'     
                    }

preempt :: SimulationState -> SBlock -> Transaction -> IO SimulationState
preempt ss sb t = return $ preempt' ss sb t
