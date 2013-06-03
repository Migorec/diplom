module Simulation.HSGPSS.Simulate.Preempt where

import Simulation.HSGPSS.Blocks hiding (priority)
import Simulation.HSGPSS.Chains
import Simulation.HSGPSS.MyMaps
import Simulation.HSGPSS.Facility as F
import Simulation.HSGPSS.Transaction
import Simulation.HSGPSS.SimulationState
import Simulation.HSGPSS.MyArray
import Data.Map as DM hiding (null)

preempt' :: SimulationState -> SBlock -> Transaction -> SimulationState
preempt' ss (SBlock (PreemptIR f _ p False) ix) transact = 
    if case DM.lookup f $ facilities ss of
        Nothing -> True
        Just sf -> isAvailable sf
    then ss {facilities = defaultUpdate (F.capture (currentTime ss) (priority transact)) f $ facilities ss,
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
                                                       F.capture t (priority transact)$ 
                                                       F.release t f) f $ facilities ss, 
                     fec = fec',
                     cec = transact {currentBlock = ix, nextBlock = ix + 1, ownership = f}: cec'     
                    }
                    
preempt' _ (SBlock (PreemptIR _ Nothing _ True) _) _ = error "C Operand in PREEMPT block must be specified if E Operant is RE"

preempt' ss (SBlock (PreemptPR f _ p False) ix) transact = 
    if case DM.lookup f $ facilities ss of
        Nothing -> True
        Just sf -> isAvailable sf
    then ss {facilities = defaultUpdate (F.capture (currentTime ss) (priority transact)) f $ facilities ss,
             cec = transact{currentBlock = ix, nextBlock = ix + 1, ownership = f} : cec ss}
    else if  (ownerPriority $ facilities ss ! f) >= priority transact
         then error "PEND?!!"--ss {facilities = defaultUpdate (pend transact) f $ facilities ss}
         else let ((mt,it),fec',cec') = findInt (fec ss) (cec ss) f
                  mt' = fmap (\x -> x - currentTime ss) mt 
                  it' = case (p,mt') of
                         (Nothing,_) -> it{ownership=""}
                         (_,Nothing) -> it{ownership=""}
                         (Just n, Just t) -> it {params = defaultUpdate (\_ -> t) n $ params it,            
                                                 ownership=""}
                  t = currentTime ss
              in ss {facilities = defaultUpdate (\f -> F.setInterrupt $
                                                       F.sInterrupt (mt',it') $ 
                                                       F.capture t (priority transact)$ 
                                                       F.release t f) f $ facilities ss, 
                     fec = fec',
                     cec = transact {currentBlock = ix, nextBlock = ix + 1, ownership = f}: cec'     
                    }

preempt' ss (SBlock (PreemptPR f (Just d) p True) ix) transact =
    if case DM.lookup f $ facilities ss of
        Nothing -> True
        Just sf -> isAvailable sf
    then ss {facilities = defaultUpdate (F.capture (currentTime ss) (priority transact)) f $ facilities ss,
             cec = transact{currentBlock = ix, nextBlock = ix + 1, ownership = f} : cec ss}
    else if (ownerPriority $ facilities ss ! f) >= priority transact
         then ss {facilities = defaultUpdate (pend transact) f $ facilities ss}
         else let ((mt,it),fec',cec') = findInt (fec ss) (cec ss) f
                  mt' = fmap (\x -> x - currentTime ss) mt
                  it' = case (p,mt') of
                         (Nothing,_) -> it{ownership=""}
                         (_,Nothing) -> it{ownership=""}
                         (Just n, Just t) -> it {params = defaultUpdate (\_ -> t) n $ params it, 
                                                 ownership="",
                                                 nextBlock=d}
                  t = currentTime ss
              in ss {facilities = defaultUpdate (\f -> F.setInterrupt $
                                                       F.capture t (priority transact)$ 
                                                       F.release t f) f $ facilities ss, 
                     fec = fec',
                     cec = transact {currentBlock = ix, nextBlock = ix + 1, ownership = f}:it':cec'     
                    }

preempt' ss (SBlock (PreemptIR f (Just d) p True) ix) transact =
    if case DM.lookup f $ facilities ss of
        Nothing -> True
        Just sf -> isAvailable sf
    then ss {facilities = defaultUpdate (F.capture (currentTime ss) (priority transact)) f $ facilities ss,
             cec = transact{currentBlock = ix, nextBlock = ix + 1, ownership = f} : cec ss}
    else if  isInterrupted $ facilities ss ! f
         then ss {facilities = defaultUpdate (pend transact) f $ facilities ss}
         else let ((mt,it),fec',cec') = findInt (fec ss) (cec ss) f
                  mt' = fmap (\x -> x - currentTime ss) mt
                  it' = case (p,mt') of
                         (Nothing,_) -> it{ownership=""}
                         (_,Nothing) -> it{ownership=""}
                         (Just n, Just t) -> it {params = defaultUpdate (\_ -> t) n $ params it, 
                                                 ownership="",
                                                 nextBlock=d}
                  t = currentTime ss
              in ss {facilities = defaultUpdate (\f -> F.setInterrupt $
                                                       F.capture t (priority transact)$ 
                                                       F.release t f) f $ facilities ss, 
                     fec = fec',
                     cec = transact {currentBlock = ix, nextBlock = ix + 1, ownership = f}:it':cec'     
                    }

preempt :: SimulationState -> SBlock -> Transaction -> IO SimulationState
preempt ss sb t = return $ preempt' ss sb t

sReturn :: SimulationState -> SBlock -> Transaction -> IO SimulationState
sReturn ss sb t = return $ sReturn' ss sb t

sReturn' :: SimulationState -> SBlock -> Transaction -> SimulationState
sReturn' ss (SBlock (Return f) ix) transact = 
    let sf = facilities ss ! f
        t  = currentTime ss
    in case pc sf of
        (dt:dcs) -> ss {facilities = (defaultUpdate (\f -> F.capture t (priority dt) $ 
                                                           F.release t f{dc = dcs}) f $ facilities ss),
                        cec = transact {currentBlock = ix, nextBlock = ix + 1, ownership=""} :
                              dt {currentBlock = nextBlock dt, 
                                  nextBlock = nextBlock dt + 1, 
                                  ownership = f} :
                              cec ss
                       } 
        [] -> case ic sf of
         ((Nothing,it):ics) -> ss {facilities = (defaultUpdate 
                                                  (\f -> F.capture t (priority it) $ 
                                                         F.release t f{ic = ics,  
                                                                       isInterrupted = not $ null ics
                                                                      }) f $ facilities ss),
                                   cec = transact {currentBlock = ix, nextBlock = ix + 1, ownership=""} :
                                   it { --currentBlock = nextBlock it, 
                                        --nextBlock = nextBlock it + 1, 
                                       ownership = f} :
                                   cec ss
                       }
         ((Just dt,it):ics) -> ss {facilities = (defaultUpdate 
                                                  (\f -> F.capture t (priority it) $ 
                                                         F.release t f{ic = ics, 
                                                                       isInterrupted = not $ null ics
                                                                      }) f $ facilities ss),
                                   cec = transact {currentBlock = ix, nextBlock = ix + 1, ownership=""} :
                                   cec ss,
                                   fec = addFE (fec ss) (t + dt, it{ --currentBlock = nextBlock it, 
                                                                     --nextBlock = nextBlock it + 1, 
                                                                    ownership = f})
                       }
         [] -> case dc sf of
                [] -> ss{facilities = defaultUpdate (\f -> F.unsetInterrupt $ 
                                                           F.release (currentTime ss) f) f $ facilities ss,
                         cec = transact{currentBlock = ix, nextBlock = ix + 1, ownership = ""} : cec ss}
                t:ts -> ss{facilities = defaultUpdate (\f -> (F.capture (currentTime ss) 
                                                                         (priority transact) $ 
                                                              F.release (currentTime ss) $
                                                              F.unsetInterrupt f{dc = ts})) f $ facilities ss,
                           cec = transact{currentBlock = ix, nextBlock = ix + 1, ownership = ""} : t{nextBlock = nextBlock t + 1, currentBlock = nextBlock t, state = Active, ownership = f} : cec ss}


