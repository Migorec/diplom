{-# LANGUAGE FlexibleInstances #-}

module Simulation.HSGPSS.Blocks.Queue where

import Simulation.HSGPSS.Blocks


class QueueClass a where
    queue :: a -> BlockStateMonad
    
instance QueueClass String where
    queue s = addBlock $ Queue s 1
    
instance QueueClass (String, Double) where
    queue (s,n) = addBlock $ Queue s (round n)
    
instance QueueClass (String, Int) where
    queue (s,n) = addBlock $ Queue s n
