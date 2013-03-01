{-# LANGUAGE FlexibleInstances #-}

module Simulation.HSGPSS.Blocks.Facility where

import Simulation.HSGPSS.Blocks

seize :: String -> BlockStateMonad
seize s = addBlock $ Seize s

release :: String -> BlockStateMonad
release s = addBlock $ Release s

return' :: String -> BlockStateMonad
return' s = addBlock $ Return s

favail :: String -> BlockStateMonad
favail s = addBlock $ FAvail s

data PR = PR

class PreemptClass a where
    preempt :: a -> BlockStateMonad
    
instance PreemptClass String where
    preempt s = addBlock $ PreemptIR s Nothing Nothing False
    
instance PreemptClass (String,PR) where
    preempt (s,PR) = addBlock $ PreemptPR s Nothing Nothing False
    
instance PreemptClass (String,(),Double) where
    preempt (s,(),d) = addBlock $ PreemptIR s (Just $ round d) Nothing False
    
instance PreemptClass (String,(),Int) where
    preempt (s,(),d) = addBlock $ PreemptIR s (Just d) Nothing False

instance PreemptClass (String,PR,Double) where
    preempt (s,PR,d) = addBlock $ PreemptPR s (Just $ round d) Nothing False
    
instance PreemptClass (String,PR,Int) where
    preempt (s,PR,d) = addBlock $ PreemptPR s (Just d) Nothing False
    
instance PreemptClass (String,(),(),Double) where
    preempt (s,(),(),p) = addBlock $ PreemptIR s Nothing (Just $ round p) False
    
instance PreemptClass (String,(),(),Int) where
    preempt (s,(),(),p) = addBlock $ PreemptIR s Nothing (Just p) False
    
instance PreemptClass (String,PR,(),Double) where
    preempt (s,PR,(),p) = addBlock $ PreemptPR s Nothing (Just $ round p) False
    
instance PreemptClass (String,PR,(),Int) where
    preempt (s,PR,(),p) = addBlock $ PreemptPR s Nothing (Just p) False
    
instance PreemptClass (String,(),Double,Double) where
    preempt (s,(),d,p) = addBlock $ PreemptIR s (Just $ round d) (Just $ round p) False
    
instance PreemptClass (String,(),Int,Double) where
    preempt (s,(),d,p) = addBlock $ PreemptIR s (Just d) (Just $ round p) False
    
instance PreemptClass (String,(),Double,Int) where
    preempt (s,(),d,p) = addBlock $ PreemptIR s (Just $ round d) (Just p) False
    
instance PreemptClass (String,(),Int,Int) where
    preempt (s,(),d,p) = addBlock $ PreemptIR s (Just d) (Just p) False
    
instance PreemptClass (String,PR,Double,Double) where
    preempt (s,PR,d,p) = addBlock $ PreemptPR s (Just $ round d) (Just $ round p) False
    
instance PreemptClass (String,PR,Int,Double) where
    preempt (s,PR,d,p) = addBlock $ PreemptPR s (Just d) (Just $ round p) False
    
instance PreemptClass (String,PR,Double,Int) where
    preempt (s,PR,d,p) = addBlock $ PreemptPR s (Just $ round d) (Just p) False
    
instance PreemptClass (String,PR,Int,Int) where
    preempt (s,PR,d,p) = addBlock $ PreemptPR s (Just d) (Just p) False
    
data RE = RE
    
instance PreemptClass (String,(),Double,(),RE) where
    preempt (s,(),d,(),RE) = addBlock $ PreemptIR s (Just $ round d) Nothing True
    
instance PreemptClass (String,(),Int,(),RE) where
    preempt (s,(),d,(),RE) = addBlock $ PreemptIR s (Just d) Nothing True
    
instance PreemptClass (String,PR,Double,(),RE) where
    preempt (s,PR,d,(),RE) = addBlock $ PreemptPR s (Just $ round d) Nothing True
    
instance PreemptClass (String,PR,Int,(),RE) where
    preempt (s,PR,d,(),RE) = addBlock $ PreemptPR s (Just d) Nothing True
    
    
instance PreemptClass (String,(),Double,Double,RE) where
    preempt (s,(),d,p,RE) = addBlock $ PreemptIR s (Just $ round d) (Just $ round p) True
    
instance PreemptClass (String,(),Double,Int,RE) where
    preempt (s,(),d,p,RE) = addBlock $ PreemptIR s (Just $ round d) (Just p) True
    
instance PreemptClass (String,(),Int,Double,RE) where
    preempt (s,(),d,p,RE) = addBlock $ PreemptIR s (Just d) (Just $ round p) True
    
instance PreemptClass (String,(),Int,Int,RE) where
    preempt (s,(),d,p,RE) = addBlock $ PreemptIR s (Just d) (Just p) True
    
instance PreemptClass (String,PR,Double,Double,RE) where
    preempt (s,PR,d,p,RE) = addBlock $ PreemptPR s (Just $ round d) (Just $ round p) True
    
instance PreemptClass (String,PR,Double,Int,RE) where
    preempt (s,PR,d,p,RE) = addBlock $ PreemptPR s (Just $ round d) (Just p) True
    
instance PreemptClass (String,PR,Int,Double,RE) where
    preempt (s,PR,d,p,RE) = addBlock $ PreemptPR s (Just d) (Just $ round p) True
    
instance PreemptClass (String,PR,Int,Int,RE) where
    preempt (s,PR,d,p,RE) = addBlock $ PreemptPR s (Just d) (Just p) True
