{-# LANGUAGE FlexibleInstances #-}

module Simulation.HSGPSS.Blocks.Advance where

import Simulation.HSGPSS.Blocks

class AdvanceClass a where
    advance :: a -> BlockStateMonad
    
instance AdvanceClass Double where
    advance m = addBlock $ AdvanceRange m 0
    
instance AdvanceClass (Double, Double) where
    advance (m, r) = addBlock $ AdvanceRange m r
    
instance AdvanceClass (Double, Double -> Double) where
    advance (m, f) = addBlock $ AdvanceFunc m f
