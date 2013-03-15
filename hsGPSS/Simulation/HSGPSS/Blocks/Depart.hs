{-# LANGUAGE FlexibleInstances #-}

module Simulation.HSGPSS.Blocks.Depart where

import Simulation.HSGPSS.Blocks


class DepartClass a where
    depart :: a -> BlockStateMonad
    
instance DepartClass String where
    depart s = addBlock $ Depart s 1
    
instance DepartClass (String, Double) where
    depart (s,n) = addBlock $ Depart s (round n)
    
instance DepartClass (String, Int) where
    depart (s,n) = addBlock $ Depart s n
